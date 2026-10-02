import type { Ingrediente } from "./claude";
import { REPARTI } from "./claude";

type GiornoConIngredienti = {
  pasti: { ingredienti: Ingrediente[] }[];
};

export type GroceryItem = {
  nome: string;
  quantita: number;
  quantitaNecessaria: number;
  confezione: number | null;
  unita: Ingrediente["unita"];
  prezzo_stimato: number;
};

export type GroceryReparto = {
  reparto: string;
  items: GroceryItem[];
  subtotale: number;
};

export type RimastoItem = {
  nome: string;
  quantita: number;
  quantitaNecessaria: number;
  unita: Ingrediente["unita"];
};

export type GroceryList = {
  reparti: GroceryReparto[];
  rimasto: RimastoItem[];
  totale_stimato: number;
  fascia: "discount" | "media" | "premium";
};

export type ConsumoDispensa = {
  nome: string;
  unita: Ingrediente["unita"];
  quantita: number;
};

export type RisultatoGroceryList = {
  groceryList: GroceryList;
  consumiDispensa: ConsumoDispensa[];
};

function chiaveDispensa(nome: string, unita: string): string {
  return `${nome.toLowerCase()}__${unita}`;
}

// Il prezzo di ogni ingrediente è stimato da Claude al momento della
// generazione del piano (vedi ISTRUZIONI_INGREDIENTI in claude.ts), ingrediente
// per ingrediente — non un prezzo medio per reparto. Qui applichiamo solo
// l'aggiustamento per fascia di supermercato e l'arrotondamento alla
// confezione reale (vedi sotto). Resta comunque una stima, non un prezzo
// reale in tempo reale (coerente col principio di fiducia: nessuna
// integrazione retailer).
const TIER_MOLTIPLICATORE: Record<"discount" | "media" | "premium", number> = {
  discount: 0.8,
  media: 1.0,
  premium: 1.3,
};

const FASCIA_SUPERMERCATO: Record<string, "discount" | "media" | "premium"> = {
  Eurospin: "discount",
  Lidl: "discount",
  Conad: "media",
  Coop: "media",
  Carrefour: "media",
  Esselunga: "premium",
};

function fasciaDaSupermercato(supermercato: string | null): "discount" | "media" | "premium" {
  if (!supermercato) return "media";
  return FASCIA_SUPERMERCATO[supermercato] || "media";
}

// Al supermercato non si compra esattamente la quantità di una ricetta: si
// compra la confezione. Questa tabella stima la taglia di confezione reale
// più comune per ingredienti da dispensa/surgelati/latticini (frutta,
// verdura fresca e carne/pesce freschi restano "a peso", senza arrotondamento,
// coerente con come si vendono davvero al banco in Italia).
const CONFEZIONI: { keywords: string[]; unita: Ingrediente["unita"]; taglia: number }[] = [
  { keywords: ["pollo", "tacchino"], unita: "g", taglia: 500 },
  { keywords: ["macinato", "salsiccia", "hamburger"], unita: "g", taglia: 500 },
  { keywords: ["piselli", "spinaci", "fagiolini", "mais", "verdure miste", "minestrone"], unita: "g", taglia: 750 },
  { keywords: ["gamber"], unita: "g", taglia: 500 },
  { keywords: ["pane"], unita: "g", taglia: 400 },
  { keywords: ["pasta"], unita: "g", taglia: 500 },
  { keywords: ["riso"], unita: "g", taglia: 1000 },
  { keywords: ["farina"], unita: "g", taglia: 1000 },
  { keywords: ["zucchero"], unita: "g", taglia: 1000 },
  { keywords: ["quinoa", "cuscus", "couscous"], unita: "g", taglia: 500 },
  { keywords: ["latte"], unita: "ml", taglia: 1000 },
  { keywords: ["latte"], unita: "l", taglia: 1 },
  { keywords: ["olio"], unita: "ml", taglia: 1000 },
  { keywords: ["olio"], unita: "l", taglia: 1 },
  { keywords: ["aceto"], unita: "ml", taglia: 500 },
  { keywords: ["passata di pomodoro"], unita: "ml", taglia: 700 },
  { keywords: ["pomodori pelati", "polpa di pomodoro"], unita: "g", taglia: 400 },
  { keywords: ["tonno"], unita: "g", taglia: 160 },
  { keywords: ["ceci", "fagioli", "lenticchie"], unita: "g", taglia: 400 },
  { keywords: ["besciamella"], unita: "g", taglia: 250 },
  { keywords: ["parmigiano", "grana", "pecorino"], unita: "g", taglia: 200 },
  { keywords: ["mozzarella"], unita: "g", taglia: 125 },
  { keywords: ["feta", "gorgonzola", "taleggio"], unita: "g", taglia: 200 },
  { keywords: ["burro"], unita: "g", taglia: 250 },
  { keywords: ["panna"], unita: "ml", taglia: 200 },
  { keywords: ["uova"], unita: "pz", taglia: 6 },
];

// Fallback per reparto: se un ingrediente di questi reparti non matcha
// nessuna parola chiave sopra, si arrotonda comunque a una taglia tipica
// (quasi tutto in questi reparti è venduto in confezioni fisse).
const CONFEZIONE_DEFAULT_PER_REPARTO: Record<string, number> = {
  Surgelati: 750,
  Dispensa: 500,
  "Pane e cereali": 500,
};

function trovaConfezione(
  reparto: string,
  nome: string,
  unita: Ingrediente["unita"],
): number | null {
  const lower = nome.toLowerCase();
  const match = CONFEZIONI.find((c) => c.unita === unita && c.keywords.some((k) => lower.includes(k)));
  if (match) return match.taglia;

  if ((unita === "g" || unita === "ml") && CONFEZIONE_DEFAULT_PER_REPARTO[reparto]) {
    return CONFEZIONE_DEFAULT_PER_REPARTO[reparto];
  }

  return null;
}

/**
 * @param dispensa Saldo disponibile in dispensa, chiave `nome__unita` (minuscolo) -> quantità.
 *   Viene sottratto dal fabbisogno PRIMA di arrotondare alla confezione: se la
 *   dispensa copre già tutto il necessario, l'ingrediente non compare nella lista.
 */
export function buildGroceryList(
  giorni: GiornoConIngredienti[],
  supermercato: string | null,
  dispensa: Map<string, number> = new Map(),
): RisultatoGroceryList {
  const fascia = fasciaDaSupermercato(supermercato);
  const moltiplicatore = TIER_MOLTIPLICATORE[fascia];

  // Aggrega per (reparto, nome, unita): somma sia la quantità usata nelle
  // ricette sia il prezzo stimato da Claude per ogni occorrenza.
  const aggregato = new Map<
    string,
    { reparto: string; nome: string; unita: Ingrediente["unita"]; quantitaUsata: number; prezzo: number }
  >();

  for (const giorno of giorni) {
    for (const pasto of giorno.pasti) {
      for (const ing of pasto.ingredienti) {
        const chiave = `${ing.reparto}__${ing.nome.toLowerCase()}__${ing.unita}`;
        const esistente = aggregato.get(chiave);
        if (esistente) {
          esistente.quantitaUsata += ing.quantita;
          esistente.prezzo += ing.prezzo_stimato_eur;
        } else {
          aggregato.set(chiave, {
            reparto: ing.reparto,
            nome: ing.nome,
            unita: ing.unita,
            quantitaUsata: ing.quantita,
            prezzo: ing.prezzo_stimato_eur,
          });
        }
      }
    }
  }

  const rimasto: RimastoItem[] = [];
  const consumiDispensa: ConsumoDispensa[] = [];
  const perReparto = new Map<string, GroceryItem[]>();

  for (const { reparto, nome, unita, quantitaUsata, prezzo } of aggregato.values()) {
    const saldoDispensa = dispensa.get(chiaveDispensa(nome, unita)) || 0;
    const consumatoDallaDispensa = Math.min(saldoDispensa, quantitaUsata);
    if (consumatoDallaDispensa > 0) {
      consumiDispensa.push({ nome, unita, quantita: consumatoDallaDispensa });
    }

    const quantitaDaComprare = quantitaUsata - consumatoDallaDispensa;
    const prezzoPerUnita = quantitaUsata > 0 ? prezzo / quantitaUsata : prezzo;

    if (quantitaDaComprare <= 0) {
      // La dispensa copre già tutto il fabbisogno: niente da comprare.
      continue;
    }

    const taglia = trovaConfezione(reparto, nome, unita);

    let quantitaAcquistata = quantitaDaComprare;
    if (taglia) {
      quantitaAcquistata = Math.ceil(quantitaDaComprare / taglia) * taglia;
      const avanzo = quantitaAcquistata - quantitaDaComprare;
      if (avanzo > 0) {
        rimasto.push({ nome, quantita: avanzo, quantitaNecessaria: quantitaDaComprare, unita });
      }
    }

    // Prezzo scalato dalla quantità effettivamente usata (prima della
    // dispensa) a quella acquistata (compri la confezione intera).
    const prezzo_stimato = prezzoPerUnita * quantitaAcquistata * moltiplicatore;

    const items = perReparto.get(reparto) || [];
    items.push({
      nome,
      quantita: quantitaAcquistata,
      quantitaNecessaria: quantitaDaComprare,
      confezione: taglia,
      unita,
      prezzo_stimato,
    });
    perReparto.set(reparto, items);
  }

  const reparti: GroceryReparto[] = REPARTI.filter((r) => perReparto.has(r)).map((reparto) => {
    const items = (perReparto.get(reparto) || []).sort((a, b) => a.nome.localeCompare(b.nome));
    const subtotale = items.reduce((sum, i) => sum + i.prezzo_stimato, 0);
    return { reparto, items, subtotale };
  });

  const totale_stimato = reparti.reduce((sum, r) => sum + r.subtotale, 0);

  return {
    groceryList: {
      reparti,
      rimasto: rimasto.sort((a, b) => a.nome.localeCompare(b.nome)),
      totale_stimato,
      fascia,
    },
    consumiDispensa,
  };
}
