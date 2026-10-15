// Validazione di sicurezza per allergene/intolleranza, per ingrediente, su
// quattro livelli invece di un binario sì/no (stesso principio già usato
// per il glutine, generalizzato a più allergeni): distingue un ingrediente
// che non è ADATTO (contiene l'allergene senza ambiguità) da uno DA
// VERIFICARE (dipende dalla marca o dalla formulazione — esistono versioni
// certificate sicure). Solo il primo caso blocca il pasto e fa scattare un
// tentativo di rigenerazione (vedi piano-validazione.ts): il secondo resta
// nel piano, segnalato per un controllo dell'etichetta, invece di scartare
// un piatto che è spesso comunque sicuro.
//
// Liste statiche curate a mano (MVP), stesso principio già usato per il
// glutine. Il riconoscimento è per PAROLA CHIAVE nel nome dell'ingrediente,
// non un campo dichiarato dall'AI: non serve un dato "questo ingrediente
// contiene X" a parte, lo stesso meccanismo che oggi riconosce "farina di
// frumento" come glutine funziona identico per "mandorle" come frutta a
// guscio. Limite onesto, non nuovo rispetto al glutine: non legge etichette
// reali, può mancare tracce/contaminazioni industriali — per questo le
// liste "da verificare" sono intenzionalmente larghe, a favore della
// cautela (un falso positivo qui costa un controllo in più, un falso
// negativo costa un rischio per la salute).
//
// Questa è l'UNICA validazione di sicurezza allergeni dell'app: ogni punto
// che modifica il piano (generazione, "Proponi un piatto diverso",
// "Sostituisci"/"Non l'ho trovato", lo scambio pasti) passa da qui tramite
// validaGiorni — nessun controllo separato altrove.

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

export type Allergene = "glutine" | "frutta_a_guscio" | "uova" | "pesce_crostacei";

export const ETICHETTE_ALLERGENE: Record<Allergene, string> = {
  glutine: "Glutine",
  frutta_a_guscio: "Frutta a guscio",
  uova: "Uova",
  pesce_crostacei: "Pesce/Crostacei",
};

// Mappa dal valore esatto salvato in profiles.restrizioni (vedi
// RESTRIZIONI_OPTIONS in opzioni-profilo.ts) all'allergene gestito qui.
// Le altre restrizioni selezionabili (Lattosio, Vegetariano, Vegano) non
// hanno ancora un controllo deterministico — restano affidate solo al
// prompt AI, come tutte e quattro erano prima di questa modularizzazione.
const ALLERGENE_PER_RESTRIZIONE: Partial<Record<string, Allergene>> = {
  "Glutine (celiachia)": "glutine",
  "Frutta a guscio": "frutta_a_guscio",
  Uova: "uova",
  "Pesce/Crostacei": "pesce_crostacei",
};

/** Allergeni con un controllo deterministico attivo per questo profilo, dalle sue restrizioni dichiarate. */
export function allergeniDalProfilo(restrizioni: string[] | null | undefined): Allergene[] {
  if (!restrizioni) return [];
  return restrizioni
    .map((r) => ALLERGENE_PER_RESTRIZIONE[r])
    .filter((a): a is Allergene => Boolean(a));
}

type RegoleAllergene = {
  /** Contengono l'allergene senza ambiguità: bloccano il pasto. */
  nonAdatto: string[];
  /** Dipendono dalla marca/formulazione: segnalati, non bloccanti. */
  daVerificare: string[];
  /** Certificano esplicitamente l'assenza dell'allergene. */
  verificato: string[];
  /** Nominano una base alternativa intrinsecamente sicura. */
  infoSufficienti: string[];
};

const REGOLE: Record<Allergene, RegoleAllergene> = {
  glutine: {
    nonAdatto: [
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
    ],
    daVerificare: ["salsa di soia", "besciamella", "dado da brodo", "dado vegetale", "malto", "cuscus", "couscous", "avena"],
    verificato: ["senza glutine", "gluten free", "certificat"],
    infoSufficienti: ["di riso", "di mais", "di grano saraceno", "di ceci", "di lenticchie", "di quinoa", "di mandorle", "di canapa"],
  },
  frutta_a_guscio: {
    nonAdatto: [
      "noce",
      "noci",
      "mandorla",
      "mandorle",
      "nocciola",
      "nocciole",
      "pistacchio",
      "pistacchi",
      "anacardo",
      "anacardi",
      "pinolo",
      "pinoli",
      "pecan",
      "macadamia",
      "castagna",
      "castagne",
      "arachide",
      "arachidi",
      "latte di mandorla",
      "farina di mandorle",
      "burro di arachidi",
      "crema di nocciole",
      "pasta di pistacchio",
      "praline",
    ],
    daVerificare: ["pesto", "torrone", "biscotti", "muesli", "cereali misti", "cioccolato"],
    verificato: ["senza frutta a guscio", "senza noci", "nut free"],
    infoSufficienti: [],
  },
  uova: {
    nonAdatto: [
      "uovo",
      "uova",
      "maionese",
      "meringa",
      "zabaione",
      "pasta all'uovo",
      "frittata",
      "omelette",
      "crema pasticcera",
      "tiramisù",
      "pandoro",
      "panettone",
    ],
    daVerificare: ["pasta fresca", "pane"],
    verificato: ["senza uova", "egg free", "vegan"],
    infoSufficienti: [],
  },
  pesce_crostacei: {
    nonAdatto: [
      "pesce",
      "salmone",
      "tonno",
      "merluzzo",
      "nasello",
      "branzino",
      "orata",
      "trota",
      "acciuga",
      "alice",
      "sgombro",
      "sogliola",
      "platessa",
      "gambero",
      "gamberetto",
      "gamberi",
      "scampo",
      "scampi",
      "aragosta",
      "astice",
      "granchio",
      "cozza",
      "cozze",
      "vongola",
      "vongole",
      "calamaro",
      "calamari",
      "polpo",
      "seppia",
      "seppie",
      "totano",
      "ostrica",
      "surimi",
      "bottarga",
      "colatura di alici",
    ],
    daVerificare: ["salsa worcester", "dado di pesce"],
    verificato: ["senza pesce", "vegan", "vegetarian"],
    infoSufficienti: [],
  },
};

function categorizzaPerAllergene(ingrediente: string, allergene: Allergene): CategoriaSicurezza {
  const lower = ingrediente.toLowerCase();
  const regole = REGOLE[allergene];

  if (regole.verificato.some((q) => lower.includes(q))) return "verificato";
  if (regole.infoSufficienti.some((q) => lower.includes(q))) return "informazioni_sufficienti";
  if (regole.nonAdatto.some((k) => lower.includes(k))) return "non_adatto";
  if (regole.daVerificare.some((k) => lower.includes(k))) return "da_verificare";
  return "informazioni_sufficienti";
}

/**
 * Categoria di sicurezza più severa tra tutti gli allergeni attivi per il
 * profilo (es. un ingrediente "non adatto" per uno solo degli allergeni
 * richiesti resta "non adatto" nel risultato complessivo — unione per
 * sicurezza, mai una media o il livello più permissivo).
 */
export function categorizzaIngrediente(ingrediente: string, allergeni: Allergene[]): CategoriaSicurezza {
  const ORDINE_SEVERITA: CategoriaSicurezza[] = ["non_adatto", "da_verificare", "verificato", "informazioni_sufficienti"];
  if (allergeni.length === 0) return "informazioni_sufficienti";

  let piuSevera: CategoriaSicurezza = "informazioni_sufficienti";
  for (const allergene of allergeni) {
    const categoria = categorizzaPerAllergene(ingrediente, allergene);
    if (ORDINE_SEVERITA.indexOf(categoria) < ORDINE_SEVERITA.indexOf(piuSevera)) {
      piuSevera = categoria;
    }
  }
  return piuSevera;
}

/** Ingredienti "non adatto" per almeno uno degli allergeni attivi: bloccano il pasto, fanno scattare una rigenerazione. */
export function ingredientiNonAdatti(ingredienti: string[], allergeni: Allergene[]): string[] {
  return ingredienti.filter((i) => categorizzaIngrediente(i, allergeni) === "non_adatto");
}

/** Ingredienti "da verificare": non bloccano il pasto, solo da segnalare in etichetta. */
export function ingredientiDaVerificare(ingredienti: string[], allergeni: Allergene[]): string[] {
  return ingredienti.filter((i) => categorizzaIngrediente(i, allergeni) === "da_verificare");
}

/** Unione delle due categorie che meritano attenzione (non_adatto + da_verificare). */
export function ingredientiDaSegnalare(ingredienti: string[], allergeni: Allergene[]): string[] {
  return ingredienti.filter((i) => {
    const categoria = categorizzaIngrediente(i, allergeni);
    return categoria === "non_adatto" || categoria === "da_verificare";
  });
}
