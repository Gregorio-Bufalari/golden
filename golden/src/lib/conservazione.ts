// Indicazione di conservazione tipica per categoria di alimento, basata su
// parole chiave nel nome dell'ingrediente — nessuna data precisa, nessuna
// notifica, solo testo informativo accanto a ogni voce della dispensa.

export type Categoria = "fresco" | "frigo_aperto" | "surgelato" | "dispensa";

const TESTO_PER_CATEGORIA: Record<Categoria, string> = {
  fresco: "Fresco deperibile — consuma entro 2-4 giorni",
  frigo_aperto: "In frigo, da aperto — si conserva 5-7 giorni",
  surgelato: "Surgelato — si conserva per mesi in freezer",
  dispensa: "Dispensa secca, anche da aperta — si conserva diversi mesi (verifica la scadenza)",
};

// Controllati per primi: lo stato indicato nel nome (surgelato, già cotto...)
// conta più della categoria base dell'ingrediente — es. "merluzzo surgelato"
// non è deperibile come il pesce fresco, "fagioli lessati" non si conservano
// come i fagioli secchi.
const INDICATORI_STATO: { keywords: string[]; categoria: Categoria }[] = [
  { keywords: ["surgelat", "congelat"], categoria: "surgelato" },
  {
    keywords: ["lessat", "lesso", "lessi", "cotto", "cotta", "cotti", "cotte", "bollito", "bollita", "bolliti", "bollite", "grigliat", "arrostit", "al naturale", "in scatola"],
    categoria: "frigo_aperto",
  },
];

const CATEGORIE_BASE: { keywords: string[]; categoria: Categoria }[] = [
  {
    keywords: ["pasta", "riso", "farina", "zucchero", "quinoa", "cuscus", "couscous", "olio", "aceto", "passata", "pomodori pelati", "polpa di pomodoro", "tonno", "ceci", "fagioli", "lenticchie"],
    categoria: "dispensa",
  },
  {
    keywords: ["pollo", "tacchino", "macinato", "salsiccia", "hamburger", "carne", "pesce", "merluzzo", "salmone", "pane"],
    categoria: "fresco",
  },
  {
    // Frutta e verdura fresca: mai arrotondata a confezione (si vende
    // sfusa), quindi non compare mai nel Frigo — ma compare nella lista
    // della spesa, dove questa categoria va riconosciuta comunque.
    keywords: [
      "pomodor", "zucchin", "insalata", "lattuga", "carot", "cipoll", "aglio",
      "peperon", "melanzan", "patat", "broccol", "cavol", "finocchi", "sedano",
      "cetriol", "rucola", "mela", "mele", "banana", "arancia", "pera", "limone",
      "fragol", "uva", "kiwi", "avocado", "lime", "basilico", "prezzemolo", "funghi",
    ],
    categoria: "fresco",
  },
  {
    keywords: ["gamber", "piselli", "spinaci", "fagiolini", "mais", "verdure miste", "minestrone"],
    categoria: "surgelato",
  },
  {
    keywords: ["latte", "panna", "besciamella", "mozzarella", "parmigiano", "grana", "pecorino", "feta", "gorgonzola", "taleggio", "burro", "uova", "yogurt"],
    categoria: "frigo_aperto",
  },
];

// Tra le keyword che matchano (sono sottostringa del nome), vince quella più
// LUNGA, non la prima della lista — a parità di lunghezza, vince l'ordine
// della lista (prima gruppo, poi keyword dentro il gruppo). Una keyword più
// lunga è quasi sempre più specifica e quindi più corretta quando due
// keyword di categorie diverse sono entrambe sottostringhe dello stesso
// nome: es. "Fagiolini" contiene sia "fagioli" (dispensa, 7 lettere) sia
// "fagiolini" (surgelato, 9 lettere) — deve vincere "fagiolini" (più
// specifica), non la prima trovata scorrendo l'array. Prima di questo
// meccanismo, l'ordine fisso dell'array decideva sempre, un bug reale
// trovato due volte: "pomodor" (fresco) intercettava "Passata di pomodoro"/
// "Pomodori pelati"/"Polpa di pomodoro" (dispensa) prima che quella
// categoria venisse mai raggiunta (corretto spostando dispensa prima — ma
// quel riordino ha introdotto esattamente lo stesso bug al contrario su
// "fagiolini"/"fagioli", scoperto con una verifica sistematica delle
// collisioni tra tutte le keyword, 6 novembre 2026). "Longest match wins"
// risolve entrambi i casi senza dover ordinare manualmente le categorie.
function trovaCategoria(lower: string, liste: { keywords: string[]; categoria: Categoria }[]): Categoria | null {
  let migliore: { categoria: Categoria; lunghezza: number } | null = null;

  for (const gruppo of liste) {
    for (const k of gruppo.keywords) {
      if (!lower.includes(k)) continue;
      if (!migliore || k.length > migliore.lunghezza) {
        migliore = { categoria: gruppo.categoria, lunghezza: k.length };
      }
    }
  }

  return migliore ? migliore.categoria : null;
}

/** Categoria di conservazione riconosciuta, o null se l'ingrediente non è in nessuna lista. */
export function categoriaConservazione(nomeIngrediente: string): Categoria | null {
  const lower = nomeIngrediente.toLowerCase();

  // Priorità FISSA tra i due gruppi di stato (surgelato prima, poi
  // frigo_aperto-da-stato) — non "keyword più lunga vince" come per la
  // categoria base sotto: un ingrediente surgelato deve restare surgelato
  // anche se il nome contiene ANCHE una frase più lunga di un altro gruppo
  // di stato, es. "Filetti di merluzzo surgelati al naturale" contiene sia
  // "surgelat" (8 lettere) sia "al naturale" (11 lettere, gruppo
  // frigo_aperto) — deve vincere comunque "surgelato", il segnale più forte
  // per la conservazione. Trovato riapplicando qui per uniformità il
  // meccanismo "keyword più lunga" introdotto sotto per CATEGORIE_BASE: ha
  // rotto questo test esistente, verifica sistematica prima del commit.
  const stato = INDICATORI_STATO.find((c) => c.keywords.some((k) => lower.includes(k)));
  if (stato) return stato.categoria;

  return trovaCategoria(lower, CATEGORIE_BASE);
}

export function conservazioneTipica(nomeIngrediente: string): string {
  const categoria = categoriaConservazione(nomeIngrediente);
  if (categoria) return TESTO_PER_CATEGORIA[categoria];
  return "Controlla la data di scadenza sulla confezione";
}

// Il colore del pallino nella tab Frigo NON vive qui: deve riflettere i
// giorni REALI rimanenti alla scadenza stimata (vedi coloreUrgenza in
// scadenza-frigo.ts), non solo la categoria statica dell'ingrediente —
// altrimenti un fresco appena comprato (4 giorni di margine) e uno in
// scadenza domani mostrerebbero sempre lo stesso colore.
export type ColoreScadenza = "rosso" | "arancione" | "verde";

export type GruppoAcquisto = "subito" | "puo_aspettare";

/**
 * Raggruppa un ingrediente per urgenza d'acquisto, riusando la stessa
 * classificazione di conservazione della tab Frigo: freschi deperibili e
 * latticini/uova "da comprare subito"; surgelati, dispensa secca e
 * ingredienti non riconosciuti (di solito spezie/condimenti da dispensa)
 * "può aspettare".
 */
export function gruppoAcquisto(nomeIngrediente: string): GruppoAcquisto {
  const categoria = categoriaConservazione(nomeIngrediente);
  if (categoria === "fresco" || categoria === "frigo_aperto") return "subito";
  return "puo_aspettare";
}
