// Validazione di sicurezza glutine per ingrediente, su quattro livelli
// invece di un binario sì/no: distingue un ingrediente che non è
// ADATTO (contiene glutine senza ambiguità, es. "farina di frumento",
// "pane") da uno DA VERIFICARE (dipende dalla marca o dalla
// formulazione, es. "dado vegetale", "salsa di soia" — esistono
// versioni certificate senza glutine). Solo il primo caso blocca il
// pasto e fa scattare un tentativo di rigenerazione (vedi
// piano-validazione.ts): il secondo resta nel piano, segnalato per un
// controllo dell'etichetta, invece di scartare un piatto che è spesso
// comunque sicuro.
//
// Lista statica curata a mano (MVP). In una fase successiva verrà
// sostituita da una fonte validata (Prontuario AIC / CREA). Questa è
// l'UNICA validazione di sicurezza glutine dell'app: ogni punto che
// modifica il piano (generazione, "Proponi un piatto diverso",
// "Sostituisci"/"Non l'ho trovato", lo scambio pasti) passa da qui
// tramite validaGiorni — nessun controllo separato altrove.

export type CategoriaSicurezza =
  | "verificato"
  | "informazioni_sufficienti"
  | "da_verificare"
  | "non_adatto";

export const ETICHETTE_CATEGORIA: Record<CategoriaSicurezza, string> = {
  verificato: "Verificato",
  informazioni_sufficienti: "Informazioni sufficienti",
  da_verificare: "Da verificare",
  non_adatto: "Non adatto",
};

// Contengono glutine senza ambiguità: nessuna marca o formulazione li
// rende sicuri, a meno di un qualificatore esplicito (vedi sotto).
const KEYWORDS_NON_ADATTO = [
  "frumento",
  "grano tenero",
  "grano duro",
  "orzo",
  "segale",
  "farina 00",
  "farina di frumento",
  "farina di grano",
  "pane",
  "pasta",
  "farro",
  "kamut",
  "seitan",
];

// Il glutine dipende dalla marca/formulazione: vale la pena controllare
// l'etichetta, ma non sono scartati a priori come i precedenti.
const KEYWORDS_DA_VERIFICARE = [
  "salsa di soia",
  "besciamella",
  "dado da brodo",
  "dado vegetale",
  "malto",
  "cuscus",
  "couscous",
  "avena",
];

// Certificano esplicitamente l'assenza di glutine: il livello di
// confidenza più alto.
const QUALIFICATORI_VERIFICATO = ["senza glutine", "gluten free", "certificat"];

// Nominano una base alternativa intrinsecamente senza glutine (es.
// "pasta di riso"): sicuro per la natura dell'ingrediente, non per una
// certificazione esplicita — stesso livello di un ingrediente che non
// solleva alcun segnale di rischio.
const QUALIFICATORI_INFO_SUFFICIENTI = [
  "di riso",
  "di mais",
  "di grano saraceno",
  "di ceci",
  "di lenticchie",
  "di quinoa",
  "di mandorle",
  "di canapa",
];

/**
 * Categorizza un ingrediente su quattro livelli di sicurezza glutine,
 * dal più al meno sicuro: "verificato" (qualificatore esplicito di
 * certificazione), "informazioni_sufficienti" (nessun segnale di
 * rischio, o sicuro per la natura dell'ingrediente), "da_verificare"
 * (possibile glutine a seconda della marca, da controllare in
 * etichetta) e "non_adatto" (contiene glutine senza ambiguità).
 */
export function categorizzaIngrediente(ingrediente: string): CategoriaSicurezza {
  const lower = ingrediente.toLowerCase();

  if (QUALIFICATORI_VERIFICATO.some((q) => lower.includes(q))) return "verificato";
  if (QUALIFICATORI_INFO_SUFFICIENTI.some((q) => lower.includes(q))) return "informazioni_sufficienti";
  if (KEYWORDS_NON_ADATTO.some((k) => lower.includes(k))) return "non_adatto";
  if (KEYWORDS_DA_VERIFICARE.some((k) => lower.includes(k))) return "da_verificare";
  return "informazioni_sufficienti";
}

export function categorizzaIngredienti(
  ingredienti: string[],
): { nome: string; categoria: CategoriaSicurezza }[] {
  return ingredienti.map((nome) => ({ nome, categoria: categorizzaIngrediente(nome) }));
}

/** Ingredienti "non adatto": bloccano il pasto, fanno scattare una rigenerazione. */
export function ingredientiNonAdatti(ingredienti: string[]): string[] {
  return ingredienti.filter((i) => categorizzaIngrediente(i) === "non_adatto");
}

/** Ingredienti "da verificare": non bloccano il pasto, solo da segnalare in etichetta. */
export function ingredientiDaVerificare(ingredienti: string[]): string[] {
  return ingredienti.filter((i) => categorizzaIngrediente(i) === "da_verificare");
}

/** Unione delle due categorie che meritano attenzione (non_adatto + da_verificare). */
export function ingredientiDaSegnalare(ingredienti: string[]): string[] {
  return ingredienti.filter((i) => {
    const categoria = categorizzaIngrediente(i);
    return categoria === "non_adatto" || categoria === "da_verificare";
  });
}
