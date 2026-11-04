import "server-only";
import { generateMealPlan, type ProfiloPerPiano, type Giorno } from "./claude";
import { validaGiorni, adattaEntroBudget, type GiornoValidato } from "./piano-validazione";
import { includiFavoritoNelPiano, type PreferitoPerRotazione } from "./preferiti-scoperta";
import { buildGroceryList, type GroceryList, type ConsumoDispensa } from "./grocery";

// Estratto da /api/piano/generate/route.ts: logica condivisa con
// /api/piano/generate/scenario/route.ts, che genera UN SOLO scenario per
// richiesta (invece di tutti e 3 in un'unica risposta) quando il profilo ha
// un budget settimanale — vedi commento su DELTA_RISPARMIO più sotto per il
// perché.

// Percentuali relative al budget settimanale dichiarato (non al costo reale
// di Equilibrato): così i 3 target sono noti in anticipo, senza dover
// aspettare che Equilibrato finisca per calcolare gli altri due — permette
// di generare tutti e 3 gli scenari in vero parallelo, ciascuno con la sua
// chiamata HTTP indipendente, mostrando ogni scenario in UI non appena è
// pronto invece di aspettarli tutti insieme (vedi BACKLOG.md, "Bug E").
export const DELTA_RISPARMIO = 0.8;
export const DELTA_ABBONDANTE = 1.2;

export type ChiaveScenario = "risparmio" | "equilibrato" | "abbondante";

export const ETICHETTA_SCENARIO: Record<ChiaveScenario, string> = {
  risparmio: "Risparmio",
  equilibrato: "Equilibrato",
  abbondante: "Più abbondante",
};

export function targetBudgetScenario(budgetSettimanale: number, chiave: ChiaveScenario): number {
  if (chiave === "risparmio") return Math.round(budgetSettimanale * DELTA_RISPARMIO);
  if (chiave === "abbondante") return Math.round(budgetSettimanale * DELTA_ABBONDANTE);
  return budgetSettimanale;
}

export type RisultatoScenario = {
  giorni: GiornoValidato[];
  groceryList: GroceryList;
  consumiDispensa: ConsumoDispensa[];
  budgetSuperato: boolean;
};

/**
 * Genera un piano completo per un target di budget, lo valida (sicurezza +
 * varietà) e lo adatta al budget se serve — eventualmente include il
 * Preferito di rotazione scelto per lo scenario "scoperta" (passato dal
 * chiamante, mai scelto di nuovo qui: deve restare LO STESSO per tutti e 3
 * gli scenari generati nella stessa richiesta dell'utente).
 */
export async function generaScenario(
  profiloInput: ProfiloPerPiano,
  modalitaProfilo: "routine" | "scoperta",
  dispensa: Map<string, number>,
  fattore: number,
  supermercato: string | null,
  favorito: PreferitoPerRotazione | null,
  targetBudget: number | null,
): Promise<RisultatoScenario> {
  const profiloScenario: ProfiloPerPiano = { ...profiloInput, budget_settimanale: targetBudget };
  const plan = await generateMealPlan(profiloScenario, modalitaProfilo, dispensa);
  const giorniBase = await validaGiorni(profiloScenario, plan.giorni, dispensa);
  // adattaEntroBudget applica anche il controllo varietà internamente (sui
  // giorni iniziali e dopo ogni eventuale adattamento budget), non serve
  // richiamare assicuraVarieta qui.
  const adattato = await adattaEntroBudget(profiloScenario, giorniBase, supermercato, targetBudget, dispensa, fattore);

  let giorniFinali = adattato.giorni;
  let groceryList = adattato.groceryList;
  let consumiDispensa = adattato.consumiDispensa;

  if (favorito) {
    // Inserito DOPO l'adattamento budget, non prima: l'AI che riduce il
    // costo rivede liberamente tutti i pasti e potrebbe alterare anche
    // questo se fosse già presente durante quel passaggio. Ricontrolla
    // comunque la sicurezza sul pasto appena inserito (potrebbe non essere
    // mai passato da validaGiorni con QUESTE restrizioni, se salvato tra i
    // preferiti prima che cambiassero) e ricalcola la lista della spesa,
    // dato che il suo costo non ha partecipato all'ottimizzazione budget
    // sopra.
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
