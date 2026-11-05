import { NextResponse } from "next/server";
import { createAdminClient } from "@/lib/supabase/admin";
import type { ProfiloPerPiano } from "@/lib/claude";
import { leggiDispensa } from "@/lib/dispensa";
import { calcolaFattoreCalibrazionePerProfilo } from "@/lib/calibrazione-prezzi";
import { nomiPastiRecenti } from "@/lib/varieta-storico";
import type { PreferitoPerRotazione } from "@/lib/preferiti-scoperta";
import { generaScenario, targetBudgetScenario, ETICHETTA_SCENARIO, type ChiaveScenario } from "@/lib/piano-scenario";

// Genera UN SOLO scenario di budget (invece di tutti e 3 insieme come la
// route principale) — usata quando il profilo ha un budget settimanale
// impostato: il client fa 3 chiamate indipendenti e parallele a questo
// endpoint (una per scenario) invece di aspettare un'unica risposta con
// tutti e 3 pronti insieme, così ogni scenario appare in UI non appena il
// SUO è pronto. Vedi il commento su questo stesso punto in
// /api/piano/generate/route.ts.
export const maxDuration = 300;

const CHIAVI_VALIDE: ChiaveScenario[] = ["risparmio", "equilibrato", "abbondante"];

export async function POST(request: Request) {
  const { token, chiave, favorito } = await request.json();

  if (!token || typeof token !== "string") {
    return NextResponse.json({ error: "Token mancante." }, { status: 400 });
  }
  if (typeof chiave !== "string" || !CHIAVI_VALIDE.includes(chiave as ChiaveScenario)) {
    return NextResponse.json({ error: "Scenario non valido." }, { status: 400 });
  }

  const supabase = createAdminClient();

  const { data: profile, error: profileError } = await supabase
    .from("profiles")
    .select(
      "id, restrizioni, obiettivo, preferenze, tempo_max_cucina, household_size, modalita, supermercato, budget_settimanale, obiettivi_nutrizionali",
    )
    .eq("link_token", token)
    .single();

  if (profileError || !profile) {
    return NextResponse.json({ error: "Profilo non trovato." }, { status: 404 });
  }

  // Questo endpoint esiste solo per il percorso "budget impostato" (vedi
  // /api/piano/generate): senza un budget dichiarato non c'è un target
  // calcolabile per questo scenario preso da solo — serve il costo reale di
  // Equilibrato, che quel percorso calcola restando nella route principale.
  if (!profile.budget_settimanale) {
    return NextResponse.json(
      { error: "Nessun budget impostato: usa /api/piano/generate per questo profilo." },
      { status: 400 },
    );
  }

  const profileId: string = profile.id;
  const supermercato = profile.supermercato;
  const modalitaProfilo = profile.modalita as "routine" | "scoperta";
  const profiloInput: ProfiloPerPiano = {
    restrizioni: profile.restrizioni || [],
    obiettivo: profile.obiettivo,
    preferenze: profile.preferenze,
    tempo_max_cucina: profile.tempo_max_cucina,
    household_size: profile.household_size,
    budget_settimanale: profile.budget_settimanale,
    obiettivi_nutrizionali: profile.obiettivi_nutrizionali,
  };

  // dispensa/calibrazione rilette da zero qui (non passate dal client): sono
  // lookup deterministici, ripeterli per ognuna delle 3 chiamate in
  // parallelo costa pochi millisecondi in più, non chiamate AI — più
  // semplice e più robusto che farle viaggiare andata e ritorno col client.
  // Il profilo stesso (restrizioni incluse) è riletto da Supabase per lo
  // stesso motivo, MAI fidato se arrivasse dal client: è il controllo di
  // sicurezza allergeni (dentro generaScenario) a dipenderne. `favorito`
  // resta l'unica eccezione, passato dal client: la sua scelta è in parte
  // casuale (vedi sceglieFavoritoScoperta) e deve restare IDENTICA per
  // tutti e 3 gli scenari della stessa richiesta — ricalcolarla qui
  // darebbe un risultato diverso a ogni chiamata. Il pasto che porta non è
  // comunque mai fidato ciecamente: generaScenario lo ripassa da
  // validaGiorni dopo averlo inserito, stessa validazione di sicurezza di
  // ogni altro pasto del piano.
  const dispensa = await leggiDispensa(supabase, profileId);
  const fattore = await calcolaFattoreCalibrazionePerProfilo(supabase, profileId, supermercato);
  // Solo in Scoperta, stesso motivo di /api/piano/generate: deterministico,
  // sicuro da ricalcolare indipendentemente in ciascuna delle 3 chiamate
  // parallele (stesso ragionamento già fatto sopra per dispensa/fattore).
  const nomiRecenti = modalitaProfilo === "scoperta" ? await nomiPastiRecenti(supabase, profileId) : [];
  const chiaveScenario = chiave as ChiaveScenario;
  const targetBudget = targetBudgetScenario(profile.budget_settimanale, chiaveScenario);

  try {
    const risultato = await generaScenario(
      profiloInput,
      modalitaProfilo,
      dispensa,
      fattore,
      supermercato,
      (favorito as PreferitoPerRotazione | null) ?? null,
      targetBudget,
      nomiRecenti,
    );

    return NextResponse.json({
      chiave: chiaveScenario,
      etichetta: ETICHETTA_SCENARIO[chiaveScenario],
      budget_target: targetBudget,
      giorni: risultato.giorni,
      grocery_list: risultato.groceryList,
      budget_superato: risultato.budgetSuperato,
    });
  } catch (err) {
    console.error(`generateMealPlan error (${chiaveScenario}):`, err);
    return NextResponse.json({ error: "Non sono riuscito a generare questo scenario. Riprova." }, { status: 502 });
  }
}
