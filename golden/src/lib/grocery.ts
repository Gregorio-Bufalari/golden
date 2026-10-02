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

// Prezzo medio al kg/litro e a pezzo, fascia "media". Stima euristica per
// l'MVP — non è un prezzo reale in tempo reale (coerente col principio di
// fiducia: nessuna integrazione retailer).
const PREZZI_BASE: Record<string, { kg: number; pz: number }> = {
  "Frutta e verdura": { kg: 2.5, pz: 0.5 },
  "Carne e pesce": { kg: 14, pz: 3 },
  "Latticini e uova": { kg: 7, pz: 0.3 },
  "Pane e cereali": { kg: 3.5, pz: 1.5 },
  Dispensa: { kg: 4, pz: 2 },
  Surgelati: { kg: 9, pz: 3 },
  Altro: { kg: 5, pz: 2 },
};

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

// "Surgelati" copre sia verdure surgelate (economiche) sia pesce/carne
// surgelati (molto più cari): senza questa distinzione il prezzo medio del
// reparto sottostima pesantemente pesce/carne surgelati.
const CARNE_PESCE_KEYWORDS = [
  "pesce",
  "merluzzo",
  "salmone",
  "orata",
  "branzino",
  "trota",
  "tonno",
  "gamber",
  "vongol",
  "cozz",
  "calamar",
  "polpa",
  "pollo",
  "tacchino",
  "manzo",
  "maiale",
  "vitello",
  "macinato",
  "salsiccia",
  "wurstel",
  "hamburger",
  "spezzatino",
];

function isCarneOPesce(nome: string): boolean {
  const lower = nome.toLowerCase();
  return CARNE_PESCE_KEYWORDS.some((k) => lower.includes(k));
}

function stimaPrezzo(
  reparto: string,
  nome: string,
  quantita: number,
  unita: Ingrediente["unita"],
  fascia: "discount" | "media" | "premium",
): number {
  const base =
    reparto === "Surgelati" && isCarneOPesce(nome)
      ? PREZZI_BASE["Carne e pesce"]
      : PREZZI_BASE[reparto] || PREZZI_BASE.Altro;
  const moltiplicatore = TIER_MOLTIPLICATORE[fascia];

  let prezzoBase: number;
  if (unita === "g") {
    prezzoBase = (quantita / 1000) * base.kg;
  } else if (unita === "kg") {
    prezzoBase = quantita * base.kg;
  } else if (unita === "ml") {
    prezzoBase = (quantita / 1000) * base.kg;
  } else if (unita === "l") {
    prezzoBase = quantita * base.kg;
  } else {
    // pz o confezione
    prezzoBase = quantita * base.pz;
  }

  return prezzoBase * moltiplicatore;
}

export function buildGroceryList(
  giorni: GiornoConIngredienti[],
  supermercato: string | null,
): GroceryList {
  const fascia = fasciaDaSupermercato(supermercato);

  // Aggrega per (reparto, nome, unita)
  const aggregato = new Map<string, { reparto: string; nome: string; unita: Ingrediente["unita"]; quantita: number }>();

  for (const giorno of giorni) {
    for (const pasto of giorno.pasti) {
      for (const ing of pasto.ingredienti) {
        const chiave = `${ing.reparto}__${ing.nome.toLowerCase()}__${ing.unita}`;
        const esistente = aggregato.get(chiave);
        if (esistente) {
          esistente.quantita += ing.quantita;
        } else {
          aggregato.set(chiave, {
            reparto: ing.reparto,
            nome: ing.nome,
            unita: ing.unita,
            quantita: ing.quantita,
          });
        }
      }
    }
  }

  const perReparto = new Map<string, GroceryItem[]>();
  for (const { reparto, nome, unita, quantita } of aggregato.values()) {
    const prezzo_stimato = stimaPrezzo(reparto, nome, quantita, unita, fascia);
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
