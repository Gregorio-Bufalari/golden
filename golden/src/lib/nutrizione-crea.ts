// Valori nutrizionali ufficiali CREA/INRAN (Centro di ricerca Alimenti e
// Nutrizione), per 100g di parte edibile — tabella statica fornita dal
// founder, nessuna chiamata esterna. L'Aglio è volutamente escluso (valore
// trovato non attendibile): un piatto che lo usa resta "stima AI" come
// qualunque altro ingrediente non coperto da questa tabella, non è
// un'omissione (13 ottobre 2026). Il Prontuario AIC resta fuori scope.
//
// Citazione obbligatoria accanto a ogni dato mostrato (condizione d'uso di
// questi dati, non facoltativa) — vedi CITAZIONE_CREA, usata in UI ovunque
// fonte_nutrizione === "crea_inran".
export const CITAZIONE_CREA = "Fonte: CREA/INRAN - Centro di ricerca Alimenti e Nutrizione";

export type ValoriNutrizionali = {
  calorie: number;
  proteine_g: number;
  carboidrati_g: number;
  grassi_g: number;
  fibre_g: number;
};

type VoceCrea = {
  /** Nome ufficiale della riga CREA/INRAN, solo per riferimento/debug. */
  nome: string;
  /** Match per parola intera (non sottostringa) sul nome normalizzato dell'ingrediente. */
  keywords: string[];
  /** Parole che, se presenti, escludono il match (es. "latte di mandorla" non è "Latte di vacca"). */
  esclusioni?: string[];
  valori: ValoriNutrizionali;
};

function normalizza(testo: string): string {
  return testo
    .normalize("NFD")
    .replace(/[̀-ͯ]/g, "")
    .toLowerCase();
}

function tokenizza(testo: string): Set<string> {
  return new Set(normalizza(testo).split(/[^a-z]+/).filter(Boolean));
}

// Match per parola intera, non per sottostringa: "fagioli" non deve
// combaciare con "fagiolini" (verdura fresca, valori molto diversi dai
// legumi secchi) solo perché la seconda CONTIENE la prima come sequenza di
// caratteri — stesso motivo per cui "mela" non deve combaciare con
// "melanzana". Le esclusioni coprono le varianti più comuni (es. latte/
// yogurt vegetali, patate dolci) dove il nome generico combacerebbe ma il
// prodotto reale ha un profilo nutrizionale nettamente diverso da quello
// coperto da questa riga.
const TABELLA_CREA: VoceCrea[] = [
  { nome: "Riso, brillato", keywords: ["riso"], valori: { calorie: 334, proteine_g: 6.7, carboidrati_g: 80.4, grassi_g: 0.4, fibre_g: 1.0 } },
  { nome: "Farina di frumento, tipo 00", keywords: ["farina"], valori: { calorie: 323, proteine_g: 11.0, carboidrati_g: 71.6, grassi_g: 0.7, fibre_g: 2.2 } },
  { nome: "Pane bianco", keywords: ["pane"], valori: { calorie: 268, proteine_g: 8.1, carboidrati_g: 59.5, grassi_g: 0.5, fibre_g: 3.8 } },
  { nome: "Pasta di semola", keywords: ["pasta"], valori: { calorie: 341, proteine_g: 13.5, carboidrati_g: 72.7, grassi_g: 1.2, fibre_g: 1.7 } },
  { nome: "Ceci, secchi", keywords: ["ceci", "cece"], valori: { calorie: 343, proteine_g: 20.9, carboidrati_g: 46.9, grassi_g: 6.3, fibre_g: 13.6 } },
  { nome: "Fagioli, secchi", keywords: ["fagioli", "fagiolo"], valori: { calorie: 326, proteine_g: 23.6, carboidrati_g: 47.5, grassi_g: 2.0, fibre_g: 17.5 } },
  { nome: "Lenticchie, secche", keywords: ["lenticchie", "lenticchia"], valori: { calorie: 319, proteine_g: 22.7, carboidrati_g: 51.1, grassi_g: 1.0, fibre_g: 13.8 } },
  { nome: "Carote, crude", keywords: ["carote", "carota"], valori: { calorie: 41, proteine_g: 1.1, carboidrati_g: 7.6, grassi_g: 0.2, fibre_g: 3.1 } },
  { nome: "Cipolle, crude", keywords: ["cipolle", "cipolla"], valori: { calorie: 28, proteine_g: 1.0, carboidrati_g: 5.7, grassi_g: 0.1, fibre_g: 1.0 } },
  { nome: "Lattuga, fresca", keywords: ["lattuga"], valori: { calorie: 22, proteine_g: 1.8, carboidrati_g: 2.2, grassi_g: 0.4, fibre_g: 1.5 } },
  { nome: "Spinaci, crudi", keywords: ["spinaci"], valori: { calorie: 35, proteine_g: 3.4, carboidrati_g: 2.9, grassi_g: 0.7, fibre_g: 1.9 } },
  { nome: "Patate, crude", keywords: ["patate", "patata"], esclusioni: ["dolci", "dolce", "americana", "americane"], valori: { calorie: 72, proteine_g: 2.0, carboidrati_g: 16.0, grassi_g: 0.1, fibre_g: 1.8 } },
  { nome: "Pomodori, maturi, freschi", keywords: ["pomodori", "pomodoro"], esclusioni: ["secchi", "secco", "passata"], valori: { calorie: 23, proteine_g: 1.0, carboidrati_g: 3.5, grassi_g: 0.2, fibre_g: 2.0 } },
  { nome: "Zucchine, chiare crude", keywords: ["zucchine", "zucchina"], valori: { calorie: 11, proteine_g: 1.3, carboidrati_g: 1.4, grassi_g: 0.1, fibre_g: 1.2 } },
  { nome: "Mele, fresche, golden", keywords: ["mele", "mela"], valori: { calorie: 46, proteine_g: 0.4, carboidrati_g: 10.7, grassi_g: 0.1, fibre_g: 1.7 } },
  { nome: "Banane, fresche", keywords: ["banane", "banana"], valori: { calorie: 76, proteine_g: 1.2, carboidrati_g: 17.4, grassi_g: 0.3, fibre_g: 1.8 } },
  { nome: "Arance, fresche", keywords: ["arance", "arancia"], valori: { calorie: 37, proteine_g: 0.7, carboidrati_g: 7.8, grassi_g: 0.2, fibre_g: 1.6 } },
  { nome: "Limoni, freschi", keywords: ["limoni", "limone"], valori: { calorie: 16, proteine_g: 0.6, carboidrati_g: 2.5, grassi_g: 0.0, fibre_g: 1.9 } },
  { nome: "Olio di oliva extra vergine", keywords: ["olio"], valori: { calorie: 899, proteine_g: 0.0, carboidrati_g: 0.0, grassi_g: 99.9, fibre_g: 0.0 } },
  { nome: "Bovino adulto, tagli anteriori, crudo", keywords: ["manzo", "bovino"], esclusioni: ["brodo"], valori: { calorie: 145, proteine_g: 20.5, carboidrati_g: 0.0, grassi_g: 7.0, fibre_g: 0.0 } },
  { nome: "Pollo, petto, crudo", keywords: ["pollo"], esclusioni: ["brodo"], valori: { calorie: 100, proteine_g: 23.3, carboidrati_g: 0.0, grassi_g: 0.8, fibre_g: 0.0 } },
  { nome: "Merluzzo o nasello", keywords: ["merluzzo", "nasello"], valori: { calorie: 71, proteine_g: 17.0, carboidrati_g: 0.0, grassi_g: 0.3, fibre_g: 0.0 } },
  { nome: "Salmone", keywords: ["salmone"], valori: { calorie: 185, proteine_g: 18.4, carboidrati_g: 1.0, grassi_g: 12.0, fibre_g: 0.0 } },
  { nome: "Tonno", keywords: ["tonno"], valori: { calorie: 159, proteine_g: 21.5, carboidrati_g: 0.1, grassi_g: 8.1, fibre_g: 0.0 } },
  { nome: "Latte di vacca, pastorizzato, intero", keywords: ["latte"], esclusioni: ["mandorla", "soia", "soya", "avena", "cocco", "riso"], valori: { calorie: 64, proteine_g: 3.3, carboidrati_g: 4.9, grassi_g: 3.6, fibre_g: 0.0 } },
  { nome: "Mozzarella di vacca", keywords: ["mozzarella"], esclusioni: ["bufala"], valori: { calorie: 253, proteine_g: 18.7, carboidrati_g: 0.7, grassi_g: 19.5, fibre_g: 0.0 } },
  { nome: "Parmigiano Reggiano DOP", keywords: ["parmigiano"], valori: { calorie: 397, proteine_g: 32.4, carboidrati_g: 0.0, grassi_g: 29.7, fibre_g: 0.0 } },
  { nome: "Burro", keywords: ["burro"], esclusioni: ["arachidi", "arachide", "mandorle", "mandorla"], valori: { calorie: 758, proteine_g: 0.8, carboidrati_g: 1.1, grassi_g: 83.4, fibre_g: 0.0 } },
  { nome: "Uova di gallina, intero", keywords: ["uova", "uovo"], valori: { calorie: 128, proteine_g: 12.4, carboidrati_g: 0.0, grassi_g: 8.7, fibre_g: 0.0 } },
  { nome: "Yogurt da latte, intero", keywords: ["yogurt"], esclusioni: ["soia", "vegetale", "cocco", "mandorla", "greco"], valori: { calorie: 66, proteine_g: 3.8, carboidrati_g: 4.3, grassi_g: 3.9, fibre_g: 0.0 } },
];

/** Riga CREA/INRAN che combacia con il nome ingrediente, o null se non coperto. */
export function cercaValoriCrea(nomeIngrediente: string): VoceCrea | null {
  const tokens = tokenizza(nomeIngrediente);
  for (const voce of TABELLA_CREA) {
    const combacia = voce.keywords.some((k) => tokens.has(k));
    if (!combacia) continue;
    const escluso = voce.esclusioni?.some((e) => tokens.has(e));
    if (escluso) continue;
    return voce;
  }
  return null;
}

// Solo g/kg sono convertibili in grammi senza assumere una densità non
// fornita: un ingrediente in ml/l/pz/confezione non entra mai nel calcolo
// CREA, anche se il nome combacerebbe (fa restare l'intero piatto "stima
// AI", per la regola "non mischiare le due fonti nello stesso piatto").
function pesoInGrammi(quantita: number, unita: string): number | null {
  if (unita === "g") return quantita;
  if (unita === "kg") return quantita * 1000;
  return null;
}

function arrotonda1(n: number): number {
  return Math.round(n * 10) / 10;
}

export type FonteNutrizione = "crea_inran" | "stima_ai";

/**
 * Nutrizione di un piatto: dato CREA/INRAN se e solo se OGNI ingrediente è
 * coperto dalla tabella ed espresso in g/kg, altrimenti la stima AI
 * originale — mai mischiate nello stesso piatto (vedi `cercaValoriCrea` e
 * `pesoInGrammi`). Quando il dato è CREA/INRAN, è calcolato scalando i
 * valori per 100g sulla quantità reale di ogni ingrediente e sommando,
 * sostituendo la stima dell'AI con un numero verificabile.
 */
export function calcolaNutrizionePasto(
  ingredienti: { nome: string; quantita: number; unita: string }[],
  nutrizioneAI: ValoriNutrizionali,
): { nutrizione: ValoriNutrizionali; fonte: FonteNutrizione } {
  if (ingredienti.length === 0) return { nutrizione: nutrizioneAI, fonte: "stima_ai" };

  const totale: ValoriNutrizionali = { calorie: 0, proteine_g: 0, carboidrati_g: 0, grassi_g: 0, fibre_g: 0 };

  for (const ing of ingredienti) {
    const grammi = pesoInGrammi(ing.quantita, ing.unita);
    if (grammi === null) return { nutrizione: nutrizioneAI, fonte: "stima_ai" };

    const voce = cercaValoriCrea(ing.nome);
    if (!voce) return { nutrizione: nutrizioneAI, fonte: "stima_ai" };

    const fattore = grammi / 100;
    totale.calorie += voce.valori.calorie * fattore;
    totale.proteine_g += voce.valori.proteine_g * fattore;
    totale.carboidrati_g += voce.valori.carboidrati_g * fattore;
    totale.grassi_g += voce.valori.grassi_g * fattore;
    totale.fibre_g += voce.valori.fibre_g * fattore;
  }

  return {
    nutrizione: {
      calorie: Math.round(totale.calorie),
      proteine_g: arrotonda1(totale.proteine_g),
      carboidrati_g: arrotonda1(totale.carboidrati_g),
      grassi_g: arrotonda1(totale.grassi_g),
      fibre_g: arrotonda1(totale.fibre_g),
    },
    fonte: "crea_inran",
  };
}
