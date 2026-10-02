import type { Ingrediente } from "./claude";
import { REPARTI } from "./claude";

type GiornoConIngredienti = {
  pasti: { ingredienti: Ingrediente[] }[];
};

export type GroceryItem = {
  nome: string;
  quantita: number;
  unita: Ingrediente["unita"];
  prezzo_stimato: number;
};

export type GroceryReparto = {
  reparto: string;
  items: GroceryItem[];
  subtotale: number;
};

export type GroceryList = {
  reparti: GroceryReparto[];
  totale_stimato: number;
  fascia: "discount" | "media" | "premium";
};

// Il prezzo di ogni ingrediente è stimato da Claude al momento della
// generazione del piano (vedi ISTRUZIONI_INGREDIENTI in claude.ts), ingrediente
// per ingrediente — non un prezzo medio per reparto, che confonderebbe es.
// pesce surgelato e verdure surgelate. Qui applichiamo solo l'aggiustamento
// per fascia di supermercato. Resta comunque una stima, non un prezzo reale
// in tempo reale (coerente col principio di fiducia: nessuna integrazione
// retailer).
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

export function buildGroceryList(
  giorni: GiornoConIngredienti[],
  supermercato: string | null,
): GroceryList {
  const fascia = fasciaDaSupermercato(supermercato);
  const moltiplicatore = TIER_MOLTIPLICATORE[fascia];

  // Aggrega per (reparto, nome, unita): somma sia la quantità (per la
  // visualizzazione) sia il prezzo stimato da Claude per ogni occorrenza.
  const aggregato = new Map<
    string,
    { reparto: string; nome: string; unita: Ingrediente["unita"]; quantita: number; prezzo: number }
  >();

  for (const giorno of giorni) {
    for (const pasto of giorno.pasti) {
      for (const ing of pasto.ingredienti) {
        const chiave = `${ing.reparto}__${ing.nome.toLowerCase()}__${ing.unita}`;
        const esistente = aggregato.get(chiave);
        if (esistente) {
          esistente.quantita += ing.quantita;
          esistente.prezzo += ing.prezzo_stimato_eur;
        } else {
          aggregato.set(chiave, {
            reparto: ing.reparto,
            nome: ing.nome,
            unita: ing.unita,
            quantita: ing.quantita,
            prezzo: ing.prezzo_stimato_eur,
          });
        }
      }
    }
  }

  const perReparto = new Map<string, GroceryItem[]>();
  for (const { reparto, nome, unita, quantita, prezzo } of aggregato.values()) {
    const prezzo_stimato = prezzo * moltiplicatore;
    const items = perReparto.get(reparto) || [];
    items.push({ nome, quantita, unita, prezzo_stimato });
    perReparto.set(reparto, items);
  }

  const reparti: GroceryReparto[] = REPARTI.filter((r) => perReparto.has(r)).map((reparto) => {
    const items = (perReparto.get(reparto) || []).sort((a, b) => a.nome.localeCompare(b.nome));
    const subtotale = items.reduce((sum, i) => sum + i.prezzo_stimato, 0);
    return { reparto, items, subtotale };
  });

  const totale_stimato = reparti.reduce((sum, r) => sum + r.subtotale, 0);

  return { reparti, totale_stimato, fascia };
}
