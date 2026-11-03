import { NextResponse } from "next/server";
import { createAdminClient } from "@/lib/supabase/admin";
import { generateMealPlan, type ProfiloPerPiano, type Giorno } from "@/lib/claude";
import { validaGiorni, adattaEntroBudget, type GiornoValidato } from "@/lib/piano-validazione";
import { leggiDispensa } from "@/lib/dispensa";
import { calcolaFattoreCalibrazionePerProfilo } from "@/lib/calibrazione-prezzi";
import {
  leggiPreferitiPerRotazione,
  sceglieFavoritoScoperta,
  includiFavoritoNelPiano,
  type PreferitoPerRotazione,
} from "@/lib/preferiti-scoperta";
import { buildGroceryList, type GroceryList, type ConsumoDispensa } from "@/lib/grocery";

// Generare un piano può richiedere diverse chiamate a Claude in sequenza
// (generazione, eventuali rigenerazioni per il glutine, adattamento al
// budget) per OGNI scenario di budget (vedi sotto): il limite di default
// di Vercel per una funzione serverless è troppo basso e interromperebbe
// la richiesta a metà.
//
// Resta a 300 (non alzato) deliberatamente: Vercel Pro permette fino a 800s
// ma SOLO con "Fluid Compute" attivo nelle impostazioni del progetto — senza
// quello, dichiarare un maxDuration più alto può far fallire il build/deploy
// invece di limitarsi a non avere effetto. Non verificabile da qui se Fluid
// Compute è attivo sul progetto: se il timeout del "Task timed out after 300
// seconds" si ripresenta anche con la parallelizzazione dei 3 scenari sotto,
// il prossimo passo è verificare Fluid Compute con l'utente E SOLO ALLORA
// alzare questo valore.
export const maxDuration = 300;

function mondayOfThisWeek(d = new Date()): string {
  const day = d.getDay();
  const diffToMonday = day === 0 ? -6 : 1 - day;
  const monday = new Date(d);
  monday.setDate(d.getDate() + diffToMonday);
  return monday.toISOString().slice(0, 10);
}

// Scenari di budget mostrati in confronto, oltre al piano principale
// ("equilibrato"). Ogni scenario è un piano generato da zero per quel
// target (piatti e ingredienti diversi, non solo porzioni ridotte dello
// stesso piano).
//
// Se il profilo ha un budget settimanale impostato, i target di Risparmio/
// Abbondante sono calcolati direttamente da QUEL budget — questo permette
// di lanciare tutti e 3 gli scenari in parallelo fin da subito (vedi sotto),
// circa dimezzando il tempo totale rispetto a prima, quando Equilibrato
// doveva finire per primo per sapere il suo costo reale. Se il budget NON è
// impostato, non c'è un valore dichiarato da cui partire: i target restano
// relativi al costo EFFETTIVO di Equilibrato (comportamento originale),
// quindi Equilibrato va comunque generato per primo in quel caso.
const DELTA_RISPARMIO = 0.8;
const DELTA_ABBONDANTE = 1.2;

type RisultatoScenario = {
  giorni: GiornoValidato[];
  groceryList: GroceryList;
  consumiDispensa: ConsumoDispensa[];
  budgetSuperato: boolean;
};

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

  // Da qui in poi: generazione fresca (routine senza piano riusabile, o
  // scoperta). Genera tre scenari a budget diverso e li restituisce SENZA
  // salvarli né toccare dispensa/rotazione Preferiti: l'utente sceglie
  // quale confermare (/api/piano/conferma), solo lì si applicano gli
  // effetti collaterali e si scrive weekly_plans — altrimenti generare due
  // scenari che l'utente scarta consumerebbe comunque la dispensa o la
  // rotazione di un Preferito mai davvero usato.
  const dispensa = await leggiDispensa(supabase, profileId);
  const fattore = await calcolaFattoreCalibrazionePerProfilo(supabase, profileId, supermercato);

  // Stesso Preferito (se capita) per tutti gli scenari, così il confronto
  // varia solo per il budget — non per quale piatto preferito compare.
  let favorito: PreferitoPerRotazione | null = null;
  if (modalitaProfilo === "scoperta") {
    const preferiti = await leggiPreferitiPerRotazione(supabase, profileId);
    favorito = sceglieFavoritoScoperta(preferiti);
  }

  async function generaScenario(targetBudget: number | null): Promise<RisultatoScenario> {
    const profiloScenario: ProfiloPerPiano = { ...profiloInput, budget_settimanale: targetBudget };
    const plan = await generateMealPlan(profiloScenario, modalitaProfilo, dispensa);
    const giorniBase = await validaGiorni(profiloScenario, plan.giorni, dispensa);
    // adattaEntroBudget applica anche il controllo varietà internamente
    // (sui giorni iniziali e dopo ogni eventuale adattamento budget), non
    // serve richiamare assicuraVarieta qui.
    const adattato = await adattaEntroBudget(
      profiloScenario,
      giorniBase,
      supermercato,
      targetBudget,
      dispensa,
      fattore,
    );

    let giorniFinali = adattato.giorni;
    let groceryList = adattato.groceryList;
    let consumiDispensa = adattato.consumiDispensa;

    if (favorito) {
      // Inserito DOPO l'adattamento budget, non prima: l'AI che riduce il
      // costo rivede liberamente tutti i pasti e potrebbe alterare anche
      // questo se fosse già presente durante quel passaggio. Ricontrolla
      // comunque la sicurezza sul pasto appena inserito (potrebbe non
      // essere mai passato da validaGiorni con QUESTE restrizioni, se
      // salvato tra i preferiti prima che cambiassero) e ricalcola la
      // lista della spesa, dato che il suo costo non ha partecipato
      // all'ottimizzazione budget sopra.
      giorniFinali = includiFavoritoNelPiano(giorniFinali, favorito);
      giorniFinali = await validaGiorni(profiloScenario, giorniFinali as Giorno[], dispensa);
      const ricalcolo = buildGroceryList(giorniFinali, supermercato, dispensa, fattore);
      groceryList = ricalcolo.groceryList;
      consumiDispensa = ricalcolo.consumiDispensa;
    }

    return {
      giorni: giorniFinali,
      groceryList,
      consumiDispensa,
      budgetSuperato: Boolean(targetBudget && groceryList.totale_stimato > targetBudget),
    };
  }

  let equilibrato: RisultatoScenario;
  let targetRisparmio: number;
  let targetAbbondante: number;
  let risparmioEsito: PromiseSettledResult<RisultatoScenario>;
  let abbondanteEsito: PromiseSettledResult<RisultatoScenario>;

  if (profile.budget_settimanale) {
    // Budget impostato: i 3 target sono già noti in anticipo, nessun bisogno
    // di aspettare il costo reale di Equilibrato — tutti e 3 gli scenari
    // partono insieme.
    targetRisparmio = Math.round(profile.budget_settimanale * DELTA_RISPARMIO);
    targetAbbondante = Math.round(profile.budget_settimanale * DELTA_ABBONDANTE);

    const [equilibratoEsito, risparmioEsitoParallelo, abbondanteEsitoParallelo] = await Promise.allSettled([
      generaScenario(profile.budget_settimanale),
      generaScenario(targetRisparmio),
      generaScenario(targetAbbondante),
    ]);
    risparmioEsito = risparmioEsitoParallelo;
    abbondanteEsito = abbondanteEsitoParallelo;

    if (equilibratoEsito.status === "rejected") {
      console.error("generateMealPlan error (equilibrato):", equilibratoEsito.reason);
      return NextResponse.json({ error: "Non sono riuscito a generare il piano. Riprova." }, { status: 502 });
    }
    equilibrato = equilibratoEsito.value;
  } else {
    // Nessun budget dichiarato: i target dipendono dal costo reale di
    // Equilibrato, quindi va generato per primo (comportamento originale).
    try {
      equilibrato = await generaScenario(null);
    } catch (err) {
      console.error("generateMealPlan error (equilibrato):", err);
      return NextResponse.json({ error: "Non sono riuscito a generare il piano. Riprova." }, { status: 502 });
    }

    const baseTotale = equilibrato.groceryList.totale_stimato;
    targetRisparmio = Math.round(baseTotale * DELTA_RISPARMIO);
    targetAbbondante = Math.round(baseTotale * DELTA_ABBONDANTE);

    [risparmioEsito, abbondanteEsito] = await Promise.allSettled([
      generaScenario(targetRisparmio),
      generaScenario(targetAbbondante),
    ]);
  }

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
