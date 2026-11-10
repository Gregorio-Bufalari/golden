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
    // Controllata PRIMA delle categorie fresche qui sotto, non dopo: le sue
    // keyword sono parole intere o frasi specifiche ("passata", "pomodori
    // pelati", "polpa di pomodoro"...), mai una sottostringa generica come
    // "pomodor" — quindi un pomodoro fresco vero (es. "Pomodori",
    // "Pomodorini") non la intercetta mai per errore. Se questa categoria
    // fosse controllata DOPO "fresco" (come accadeva prima), "pomodor" nella
    // lista fresco qui sotto catturerebbe per primo "Passata di pomodoro"/
    // "Pomodori pelati"/"Polpa di pomodoro" (tutti contengono "pomodor" come
    // sottostringa), rendendo questi 3 ingredienti della lista dispensa
    // sotto irraggiungibili — bug reale riscontrato dal vivo (4 novembre
    // 2026): "Passata di pomodoro" mostrata come "Fresco deperibile —
    // consuma entro 2-4 giorni" invece di "Dispensa secca... diversi mesi".
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

/** Categoria di conservazione riconosciuta, o null se l'ingrediente non è in nessuna lista. */
export function categoriaConservazione(nomeIngrediente: string): Categoria | null {
  const lower = nomeIngrediente.toLowerCase();

  const stato = INDICATORI_STATO.find((c) => c.keywords.some((k) => lower.includes(k)));
  if (stato) return stato.categoria;

  const base = CATEGORIE_BASE.find((c) => c.keywords.some((k) => lower.includes(k)));
  if (base) return base.categoria;

  return null;
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
