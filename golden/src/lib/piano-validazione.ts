import "server-only";
import { regeneratePasto, adattaBudget, type Pasto, type Giorno, type ProfiloPerPiano } from "./claude";
import { ingredientiARischio } from "./glutine-check";
import { buildGroceryList, type GroceryList, type ConsumoDispensa } from "./grocery";

const MAX_RIGENERAZIONI = 2;
const MAX_TENTATIVI_BUDGET = 2;

export type PastoValidato = Pasto & {
  verificare?: boolean;
  ingredienti_a_rischio?: string[];
};

export type GiornoValidato = {
  giorno: string;
  pasti: PastoValidato[];
};

/**
 * Controlla ogni pasto per ingredienti a rischio glutine e rigenera quelli
 * sospetti. Le rigenerazioni di un singolo giro vengono lanciate in
 * parallelo (non un pasto alla volta in sequenza) per restare entro i
 * tempi di esecuzione della funzione su Vercel: con più pasti a rischio
 * contemporaneamente, farli uno alla volta moltiplicava i tempi di attesa.
 */
export async function validaGiorni(
  profilo: ProfiloPerPiano,
  giorni: Giorno[],
): Promise<GiornoValidato[]> {
  const richiedeControlloGlutine = profilo.restrizioni?.includes("Glutine (celiachia)");
  const pasti: PastoValidato[][] = giorni.map((g) => [...g.pasti]);

  if (richiedeControlloGlutine) {
    for (let tentativo = 0; tentativo < MAX_RIGENERAZIONI; tentativo++) {
      const daRigenerare: { gi: number; pi: number; rischi: string[] }[] = [];
      for (let gi = 0; gi < pasti.length; gi++) {
        for (let pi = 0; pi < pasti[gi].length; pi++) {
          const rischi = ingredientiARischio(pasti[gi][pi].ingredienti.map((i) => i.nome));
          if (rischi.length > 0) daRigenerare.push({ gi, pi, rischi });
        }
      }

      if (daRigenerare.length === 0) break;

      await Promise.all(
        daRigenerare.map(async ({ gi, pi, rischi }) => {
          try {
            pasti[gi][pi] = await regeneratePasto(profilo, giorni[gi].giorno, pasti[gi][pi], rischi);
          } catch (err) {
            console.error("regeneratePasto error:", err);
          }
        }),
      );
    }

    for (let gi = 0; gi < pasti.length; gi++) {
      for (let pi = 0; pi < pasti[gi].length; pi++) {
        const rischi = ingredientiARischio(pasti[gi][pi].ingredienti.map((i) => i.nome));
        if (rischi.length > 0) {
          pasti[gi][pi] = { ...pasti[gi][pi], verificare: true, ingredienti_a_rischio: rischi };
        }
      }
    }
  }

  return giorni.map((g, gi) => ({ giorno: g.giorno, pasti: pasti[gi] }));
}

export async function adattaEntroBudget(
  profilo: ProfiloPerPiano,
  giorniIniziali: GiornoValidato[],
  supermercato: string | null,
  budget: number | null,
  dispensa: Map<string, number> = new Map(),
): Promise<{
  giorni: GiornoValidato[];
  groceryList: GroceryList;
  consumiDispensa: ConsumoDispensa[];
  budgetSuperato: boolean;
}> {
  let giorni = giorniIniziali;
  let risultato = buildGroceryList(giorni, supermercato, dispensa);

  if (!budget) {
    return { giorni, groceryList: risultato.groceryList, consumiDispensa: risultato.consumiDispensa, budgetSuperato: false };
  }

  let tentativi = 0;
  while (risultato.groceryList.totale_stimato > budget && tentativi < MAX_TENTATIVI_BUDGET) {
    tentativi += 1;
    try {
      const pianoAdattato = await adattaBudget(
        profilo,
        giorni as Giorno[],
        risultato.groceryList.totale_stimato,
        budget,
      );
      giorni = await validaGiorni(profilo, pianoAdattato.giorni);
      risultato = buildGroceryList(giorni, supermercato, dispensa);
    } catch (err) {
      console.error("adattaBudget error:", err);
      break;
    }
  }

  return {
    giorni,
    groceryList: risultato.groceryList,
    consumiDispensa: risultato.consumiDispensa,
    budgetSuperato: risultato.groceryList.totale_stimato > budget,
  };
}
