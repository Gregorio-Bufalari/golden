import { NextResponse } from "next/server";
import { createAdminClient } from "@/lib/supabase/admin";
import type { ProfiloPerPiano } from "@/lib/claude";
import { leggiDispensa } from "@/lib/dispensa";
import { calcolaFattoreCalibrazionePerProfilo } from "@/lib/calibrazione-prezzi";
import { nomiPastiRecenti } from "@/lib/varieta-storico";
import { leggiPreferitiPerRotazione, sceglieFavoritoScoperta, type PreferitoPerRotazione } from "@/lib/preferiti-scoperta";
import { generaScenario, DELTA_RISPARMIO, DELTA_ABBONDANTE, type RisultatoScenario } from "@/lib/piano-scenario";
import type { GiornoValidato } from "@/lib/piano-validazione";
import type { GroceryList } from "@/lib/grocery";

// Generare un piano può richiedere diverse chiamate a Claude in sequenza
// (generazione, eventuali rigenerazioni per il glutine, adattamento al
// budget): il limite di default di Vercel per una funzione serverless è
// troppo basso e interromperebbe la richiesta a metà.
//
// Resta a 300 (non alzato) deliberatamente: Vercel Pro permette fino a 800s
// ma SOLO con "Fluid Compute" attivo nelle impostazioni del progetto — senza
// quello, dichiarare un maxDuration più alto può far fallire il build/deploy
// invece di limitarsi a non avere effetto.
export const maxDuration = 300;

function mondayOfThisWeek(d = new Date()): string {
  const day = d.getDay();
  const diffToMonday = day === 0 ? -6 : 1 - day;
  const monday = new Date(d);
  monday.setDate(d.getDate() + diffToMonday);
  return monday.toISOString().slice(0, 10);
}

export type ScenarioPiano = {
  chiave: "risparmio" | "equilibrato" | "abbondante";
  etichetta: string;
  budget_target: number | null;
  giorni: GiornoValidato[];
  grocery_list: GroceryList;
  budget_superato: boolean;
};

export async function POST(request: Request) {
  const { token } = await request.json();

  if (!token || typeof token !== "string") {
    return NextResponse.json({ error: "Token mancante." }, { status: 400 });
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

  const settimana = mondayOfThisWeek();

  // Routine con un piano riusabile già generato: nessuno scenario da
  // scegliere, si riusa direttamente come prima (nessuna chiamata AI).
  if (profile.modalita === "routine") {
    const { data: ultimoPiano } = await supabase
      .from("weekly_plans")
      .select("meal_plan, grocery_list")
      .eq("profile_id", profile.id)
      .eq("modalita_usata", "routine")
      .order("settimana", { ascending: false })
      .order("created_at", { ascending: false })
      .limit(1)
      .maybeSingle();

    const groceryListRiusabile = (ultimoPiano?.grocery_list as GroceryList | null)?.reparti;

    if (ultimoPiano?.meal_plan?.giorni && groceryListRiusabile) {
      const giorniValidati = ultimoPiano.meal_plan.giorni as GiornoValidato[];
      const groceryList = ultimoPiano.grocery_list as GroceryList;

      const { data: weeklyPlan, error: insertError } = await supabase
        .from("weekly_plans")
        .insert({
          profile_id: profile.id,
          settimana,
          meal_plan: { giorni: giorniValidati },
          modalita_usata: profile.modalita,
          budget_stimato: groceryList.totale_stimato,
          grocery_list: groceryList,
          consumi_dispensa: null,
        })
        .select("id, settimana")
        .single();

      if (insertError || !weeklyPlan) {
        console.error("weekly_plans insert error:", insertError);
        return NextResponse.json(
          { error: "Piano generato ma non sono riuscito a salvarlo." },
          { status: 500 },
        );
      }

      return NextResponse.json({
        weekly_plan_id: weeklyPlan.id,
        settimana: weeklyPlan.settimana,
        giorni: giorniValidati,
        grocery_list: groceryList,
        riusato: true,
        budget_superato: Boolean(
          profile.budget_settimanale && groceryList.totale_stimato > profile.budget_settimanale,
        ),
        budget_settimanale: profile.budget_settimanale,
      });
    }
  }

  // Stesso Preferito (se capita) per tutti gli scenari, così il confronto
  // varia solo per il budget — non per quale piatto preferito compare.
  // Scelto UNA volta qui (la scelta è in parte casuale, vedi
  // sceglieFavoritoScoperta) e riusato identico in ogni scenario, generato
  // qui sotto o dalla route /scenario per il percorso con budget.
  let favorito: PreferitoPerRotazione | null = null;
  if (modalitaProfilo === "scoperta") {
    const preferiti = await leggiPreferitiPerRotazione(supabase, profileId);
    favorito = sceglieFavoritoScoperta(preferiti);
  }

  if (profile.budget_settimanale) {
    // Budget impostato: i 3 target sono già calcolabili dal budget
    // dichiarato, senza aspettare il costo reale di Equilibrato. Invece di
    // generare tutti e 3 gli scenari qui dentro una singola richiesta (il
    // client aspetterebbe il più lento dei 3 prima di vedere qualunque
    // cosa, misurato dal vivo: ~130s), il client fa 3 chiamate HTTP
    // indipendenti e parallele a /api/piano/generate/scenario, mostrando
    // ogni scenario in UI non appena il SUO scenario è pronto (vedi
    // BACKLOG.md, "Bug E" — causa reale: adattaBudget scatta quasi sempre,
    // raddoppiando il tempo per scenario, non un problema di
    // parallelizzazione).
    return NextResponse.json({ needsScenari: true, settimana, favorito });
  }

  // Nessun budget dichiarato: non c'è un valore da cui calcolare i target
  // di Risparmio/Abbondante in anticipo, serve il costo EFFETTIVO di
  // Equilibrato — resta quindi il comportamento sequenziale originale,
  // in un'unica risposta con tutti e 3 gli scenari già pronti.
  const dispensa = await leggiDispensa(supabase, profileId);
  const fattore = await calcolaFattoreCalibrazionePerProfilo(supabase, profileId, supermercato);
  // Solo in Scoperta: Routine ripete apposta gli stessi piatti, non deve
  // evitarli (vedi commento su istruzioneStoricoRecente in claude.ts).
  const nomiRecenti = modalitaProfilo === "scoperta" ? await nomiPastiRecenti(supabase, profileId) : [];

  let equilibrato: RisultatoScenario;
  try {
    equilibrato = await generaScenario(
      profiloInput,
      modalitaProfilo,
      dispensa,
      fattore,
      supermercato,
      favorito,
      null,
      nomiRecenti,
    );
  } catch (err) {
    console.error("generateMealPlan error (equilibrato):", err);
    return NextResponse.json({ error: "Non sono riuscito a generare il piano. Riprova." }, { status: 502 });
  }

  const baseTotale = equilibrato.groceryList.totale_stimato;
  const targetRisparmio = Math.round(baseTotale * DELTA_RISPARMIO);
  const targetAbbondante = Math.round(baseTotale * DELTA_ABBONDANTE);

  const [risparmioEsito, abbondanteEsito] = await Promise.allSettled([
    generaScenario(profiloInput, modalitaProfilo, dispensa, fattore, supermercato, favorito, targetRisparmio, nomiRecenti),
    generaScenario(profiloInput, modalitaProfilo, dispensa, fattore, supermercato, favorito, targetAbbondante, nomiRecenti),
  ]);

  if (risparmioEsito.status === "rejected") {
    console.error("generateMealPlan error (risparmio):", risparmioEsito.reason);
  }
  if (abbondanteEsito.status === "rejected") {
    console.error("generateMealPlan error (abbondante):", abbondanteEsito.reason);
  }

  const candidati: { chiave: ScenarioPiano["chiave"]; etichetta: string; budgetTarget: number | null; risultato: RisultatoScenario | null }[] = [
    {
      chiave: "risparmio",
      etichetta: "Risparmio",
      budgetTarget: targetRisparmio,
      risultato: risparmioEsito.status === "fulfilled" ? risparmioEsito.value : null,
    },
    { chiave: "equilibrato", etichetta: "Equilibrato", budgetTarget: profile.budget_settimanale, risultato: equilibrato },
    {
      chiave: "abbondante",
      etichetta: "Più abbondante",
      budgetTarget: targetAbbondante,
      risultato: abbondanteEsito.status === "fulfilled" ? abbondanteEsito.value : null,
    },
  ];

  const scenari: ScenarioPiano[] = candidati
    .filter((c): c is typeof c & { risultato: RisultatoScenario } => c.risultato !== null)
    .map((c) => ({
      chiave: c.chiave,
      etichetta: c.etichetta,
      budget_target: c.budgetTarget,
      giorni: c.risultato.giorni,
      grocery_list: c.risultato.groceryList,
      budget_superato: c.risultato.budgetSuperato,
    }));

  return NextResponse.json({
    riusato: false,
    settimana,
    scenari,
    favorito_incluso_id: favorito?.id ?? null,
    budget_settimanale: profile.budget_settimanale,
  });
}
