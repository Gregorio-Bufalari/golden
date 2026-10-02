// Indicazione di conservazione tipica per categoria di alimento, basata su
// parole chiave nel nome dell'ingrediente — nessuna data precisa, nessuna
// notifica, solo testo informativo accanto a ogni voce della dispensa.

type Categoria = "fresco" | "frigo_aperto" | "surgelato" | "dispensa";

const TESTO_PER_CATEGORIA: Record<Categoria, string> = {
  fresco: "Fresco deperibile — consuma entro 2-4 giorni",
  frigo_aperto: "In frigo, da aperto — si conserva 5-7 giorni",
  surgelato: "Surgelato — si conserva per mesi in freezer",
  dispensa: "Dispensa secca — si conserva mesi o anni",
};

const CATEGORIE: { keywords: string[]; categoria: Categoria }[] = [
  {
    keywords: ["pollo", "tacchino", "macinato", "salsiccia", "hamburger", "carne", "pesce", "merluzzo", "salmone", "pane"],
    categoria: "fresco",
  },
  {
    keywords: ["gamber", "piselli", "spinaci", "fagiolini", "mais", "verdure miste", "minestrone", "surgelat"],
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

export function conservazioneTipica(nomeIngrediente: string): string {
  const lower = nomeIngrediente.toLowerCase();
  const match = CATEGORIE.find((c) => c.keywords.some((k) => lower.includes(k)));
  if (match) return TESTO_PER_CATEGORIA[match.categoria];
  return "Controlla la data di scadenza sulla confezione";
}
