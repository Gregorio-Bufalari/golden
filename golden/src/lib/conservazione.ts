// Indicazione di conservazione tipica per categoria di alimento, basata su
// parole chiave nel nome dell'ingrediente — nessuna data precisa, nessuna
// notifica, solo testo informativo accanto a ogni voce della dispensa.

type Categoria = "fresco" | "frigo_aperto" | "surgelato" | "dispensa";

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
  {
    keywords: ["pasta", "riso", "farina", "zucchero", "quinoa", "cuscus", "couscous", "olio", "aceto", "passata", "pomodori pelati", "polpa di pomodoro", "tonno", "ceci", "fagioli", "lenticchie"],
    categoria: "dispensa",
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
