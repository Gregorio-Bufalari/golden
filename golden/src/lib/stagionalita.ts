// Tabella di riferimento statica per la stagionalità di frutta e verdura in
// Italia, per mese (1 = gennaio ... 12 = dicembre). Basata sui calendari di
// stagionalità pubblici di uso comune (es. Coldiretti, Ministero della
// Salute) — un'approssimazione ragionevole, non pretende di essere
// scientificamente precisa o esaustiva: serve solo come CRITERIO IN PIÙ per
// orientare l'AI verso prodotti di stagione quando scrive il piano, non come
// vincolo. Frutta tropicale/sempre disponibile (banana, ananas, avocado...)
// è deliberatamente esclusa: non ha una vera stagione italiana.

type VoceStagionalita = { nome: string; keywords: string[]; mesi: number[] };

const CALENDARIO_STAGIONALITA: VoceStagionalita[] = [
  // Verdura
  { nome: "Carciofi", keywords: ["carciof"], mesi: [11, 12, 1, 2, 3, 4] },
  { nome: "Asparagi", keywords: ["asparag"], mesi: [3, 4, 5, 6] },
  { nome: "Broccoli", keywords: ["broccol"], mesi: [10, 11, 12, 1, 2, 3] },
  { nome: "Cavolfiori", keywords: ["cavolfior"], mesi: [9, 10, 11, 12, 1, 2, 3, 4] },
  { nome: "Cavoli/verza", keywords: ["cavolo", "cavoli", "verza"], mesi: [10, 11, 12, 1, 2, 3] },
  { nome: "Finocchi", keywords: ["finocch"], mesi: [10, 11, 12, 1, 2, 3, 4] },
  { nome: "Fave", keywords: ["fav"], mesi: [4, 5, 6] },
  { nome: "Piselli", keywords: ["piselli"], mesi: [4, 5, 6] },
  { nome: "Fagiolini", keywords: ["fagiolini"], mesi: [6, 7, 8, 9] },
  { nome: "Melanzane", keywords: ["melanzan"], mesi: [6, 7, 8, 9] },
  { nome: "Peperoni", keywords: ["peperon"], mesi: [6, 7, 8, 9, 10] },
  { nome: "Pomodori", keywords: ["pomodor"], mesi: [6, 7, 8, 9] },
  { nome: "Zucchine", keywords: ["zucchin"], mesi: [5, 6, 7, 8, 9] },
  { nome: "Zucca", keywords: ["zucca"], mesi: [9, 10, 11, 12] },
  { nome: "Spinaci", keywords: ["spinaci"], mesi: [10, 11, 12, 1, 2, 3, 4] },
  { nome: "Bietole", keywords: ["bietol"], mesi: [9, 10, 11, 12, 1, 2, 3, 4, 5] },
  { nome: "Radicchio", keywords: ["radicchio"], mesi: [10, 11, 12, 1, 2, 3] },
  { nome: "Porri", keywords: ["porr"], mesi: [10, 11, 12, 1, 2, 3] },
  { nome: "Cetrioli", keywords: ["cetriol"], mesi: [5, 6, 7, 8, 9] },
  { nome: "Sedano", keywords: ["sedano"], mesi: [9, 10, 11, 12, 1, 2] },
  { nome: "Rucola", keywords: ["rucola"], mesi: [4, 5, 6, 7, 8, 9, 10] },
  { nome: "Lattuga/insalata", keywords: ["lattuga", "insalata"], mesi: [4, 5, 6, 9, 10, 11] },
  { nome: "Carote", keywords: ["carot"], mesi: [5, 6, 7, 8, 9, 10, 11] },

  // Frutta
  { nome: "Fragole", keywords: ["fragol"], mesi: [4, 5, 6] },
  { nome: "Albicocche", keywords: ["albicocc"], mesi: [6, 7] },
  { nome: "Pesche", keywords: ["pesca", "pesche"], mesi: [6, 7, 8, 9] },
  { nome: "Susine/prugne", keywords: ["susin", "prugn"], mesi: [6, 7, 8, 9] },
  { nome: "Ciliegie", keywords: ["ciliegi"], mesi: [5, 6, 7] },
  { nome: "Meloni", keywords: ["melon"], mesi: [6, 7, 8, 9] },
  { nome: "Angurie", keywords: ["anguria", "cocomero"], mesi: [6, 7, 8, 9] },
  { nome: "Uva", keywords: ["uva"], mesi: [8, 9, 10] },
  { nome: "Fichi", keywords: ["fich"], mesi: [8, 9, 10] },
  { nome: "Mele", keywords: ["mela", "mele"], mesi: [9, 10, 11, 12, 1, 2, 3, 4] },
  { nome: "Pere", keywords: ["pera", "pere"], mesi: [8, 9, 10, 11, 12, 1] },
  { nome: "Kiwi", keywords: ["kiwi"], mesi: [11, 12, 1, 2, 3, 4, 5] },
  {
    nome: "Agrumi (arance, mandarini, clementine)",
    keywords: ["arancia", "arance", "mandarin", "clementin"],
    mesi: [11, 12, 1, 2, 3, 4],
  },
  { nome: "Pompelmo", keywords: ["pompelmo"], mesi: [12, 1, 2, 3, 4] },
  { nome: "Castagne", keywords: ["castagn"], mesi: [10, 11] },
  { nome: "Melograno", keywords: ["melograno"], mesi: [10, 11, 12] },
  { nome: "Nespole", keywords: ["nespol"], mesi: [5, 6] },
  {
    nome: "Frutti di bosco (lamponi, mirtilli, more)",
    keywords: ["lampon", "mirtill", "mora", "more"],
    mesi: [6, 7, 8, 9],
  },
];

const NOMI_MESI = [
  "gennaio",
  "febbraio",
  "marzo",
  "aprile",
  "maggio",
  "giugno",
  "luglio",
  "agosto",
  "settembre",
  "ottobre",
  "novembre",
  "dicembre",
];

/** Mesi (1-12) in cui l'ingrediente è di stagione, o null se non è in tabella. */
export function mesiDiStagione(nomeIngrediente: string): number[] | null {
  const lower = nomeIngrediente.toLowerCase();
  const voce = CALENDARIO_STAGIONALITA.find((v) => v.keywords.some((k) => lower.includes(k)));
  return voce ? voce.mesi : null;
}

/** true se l'ingrediente è di stagione nel mese indicato (1-12). */
export function isDiStagione(nomeIngrediente: string, mese: number): boolean {
  const mesi = mesiDiStagione(nomeIngrediente);
  return mesi !== null && mesi.includes(mese);
}

/** Nomi di frutta/verdura di stagione nel mese indicato (1-12). */
export function prodottiDiStagione(mese: number): string[] {
  return CALENDARIO_STAGIONALITA.filter((v) => v.mesi.includes(mese)).map((v) => v.nome);
}

/**
 * Istruzione da aggiungere al prompt dell'AI: elenca i prodotti di stagione
 * nel mese indicato (il mese corrente, di default) come criterio aggiuntivo
 * per la scelta di frutta e verdura — non un vincolo rigido. Restrizioni
 * alimentari e budget restano sempre la priorità più alta, come ricordato
 * esplicitamente nel testo stesso.
 */
export function istruzioneStagionalita(mese: number = new Date().getMonth() + 1): string {
  const prodotti = prodottiDiStagione(mese);
  if (prodotti.length === 0) return "";

  return (
    `Criterio aggiuntivo per la scelta di frutta e verdura (mai a scapito di restrizioni, sicurezza o budget): ` +
    `a ${NOMI_MESI[mese - 1]}, in Italia, sono di stagione questi prodotti — ${prodotti.join(", ")}. ` +
    "Quando scegli frutta o verdura per il piano, a parità di altre condizioni dai priorità a questi prodotti di " +
    "stagione rispetto a prodotti fuori stagione. Restrizioni alimentari e validazione di sicurezza restano SEMPRE " +
    "il vincolo più alto: la stagionalità è solo un criterio in più, mai un motivo per violarli."
  );
}
