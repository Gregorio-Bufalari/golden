import "server-only";
import { regeneratePasto, adattaBudget, type Pasto, type Giorno, type ProfiloPerPiano } from "./claude";
import { ingredientiNonAdatti, ingredientiDaVerificare } from "./glutine-check";
import { buildGroceryList, type GroceryList, type ConsumoDispensa } from "./grocery";

const MAX_RIGENERAZIONI = 2;
const MAX_TENTATIVI_BUDGET = 2;

export type PastoValidato = Pasto & {
  verificare?: boolean;
  ingredienti_a_rischio?: string[];
  // Sottoinsieme di ingredienti_a_rischio ancora "non adatto" dopo i
  // tentativi di rigenerazione (vs. solo "da verificare" in etichetta):
  // distingue in UI un blocco reale da un controllo preventivo.
  ingredienti_non_adatti?: string[];
};

export type GiornoValidato = {
  giorno: string;
  pasti: PastoValidato[];
};

/**
 * Controlla ogni pasto con la validazione a quattro livelli (vedi
 * glutine-check.ts) e rigenera solo quelli con un ingrediente "non
 * adatto" (glutine senza ambiguità): un ingrediente "da verificare"
 * (dipende dalla marca, es. dado vegetale) resta nel piano e viene solo
 * segnalato più sotto, senza sprecare un tentativo dell'AI su un
 * ingrediente che è spesso comunque sicuro. Le rigenerazioni di un
 * singolo giro vengono lanciate in parallelo (non un pasto alla volta in
 * sequenza) per restare entro i tempi di esecuzione della funzione su
 * Vercel.
 *
 * Unica validazione di sicurezza glutine dell'app: ogni punto che
 * modifica il piano (generazione, "Proponi un piatto diverso",
 * "Sostituisci"/"Non l'ho trovato" nella lista della spesa, lo scambio
 * pasti) passa da qui — nessun controllo separato altrove.
 */
export async function validaGiorni(
  profilo: ProfiloPerPiano,
  giorni: Giorno[],
  dispensa: Map<string, number> = new Map(),
): Promise<GiornoValidato[]> {
  const richiedeControlloGlutine = profilo.restrizioni?.includes("Glutine (celiachia)");
  const pasti: PastoValidato[][] = giorni.map((g) => [...g.pasti]);

  if (richiedeControlloGlutine) {
    for (let tentativo = 0; tentativo < MAX_RIGENERAZIONI; tentativo++) {
      const daRigenerare: { gi: number; pi: number; nonAdatti: string[] }[] = [];
      for (let gi = 0; gi < pasti.length; gi++) {
        for (let pi = 0; pi < pasti[gi].length; pi++) {
          const nonAdatti = ingredientiNonAdatti(pasti[gi][pi].ingredienti.map((i) => i.nome));
          if (nonAdatti.length > 0) daRigenerare.push({ gi, pi, nonAdatti });
        }
      }

      if (daRigenerare.length === 0) break;

      await Promise.all(
        daRigenerare.map(async ({ gi, pi, nonAdatti }) => {
          try {
            pasti[gi][pi] = await regeneratePasto(profilo, giorni[gi].giorno, pasti[gi][pi], nonAdatti, dispensa);
          } catch (err) {
            console.error("regeneratePasto error:", err);
          }
        }),
      );
    }

    for (let gi = 0; gi < pasti.length; gi++) {
      for (let pi = 0; pi < pasti[gi].length; pi++) {
        const nomi = pasti[gi][pi].ingredienti.map((i) => i.nome);
        const nonAdatti = ingredientiNonAdatti(nomi);
        const daVerificare = ingredientiDaVerificare(nomi);
        const daSegnalare = [...nonAdatti, ...daVerificare];
        if (daSegnalare.length > 0) {
          pasti[gi][pi] = {
            ...pasti[gi][pi],
            verificare: true,
            ingredienti_a_rischio: daSegnalare,
            ...(nonAdatti.length > 0 ? { ingredienti_non_adatti: nonAdatti } : {}),
          };
        }
      }
    }
  }

  return giorni.map((g, gi) => ({ giorno: g.giorno, pasti: pasti[gi] }));
}

const MAX_RIGENERAZIONI_VARIETA = 2;

function chiaveNomePasto(nome: string): string {
  return nome.trim().toLowerCase();
}

/**
 * Controlla che i 14 pasti della settimana siano 14 ricette distinte
 * (nessuna ripetuta) e rigenera i doppioni, passando all'AI l'elenco dei
 * piatti già usati da evitare. Senza questo controllo deterministico,
 * un'istruzione come "obiettivo: ridurre gli sprechi" può spingere l'AI a
 * collassare il piano su pochissimi piatti ripetuti — qui si tratta come un
 * vincolo verificato dopo la generazione, stesso pattern del controllo
 * glutine (static check + rigenerazione mirata, non all'infinito).
 */
export async function assicuraVarieta(
  profilo: ProfiloPerPiano,
  giorni: GiornoValidato[],
  dispensa: Map<string, number> = new Map(),
): Promise<GiornoValidato[]> {
  const pasti: PastoValidato[][] = giorni.map((g) => [...g.pasti]);

  for (let tentativo = 0; tentativo < MAX_RIGENERAZIONI_VARIETA; tentativo++) {
    const viste = new Set<string>();
    const daRigenerare: { gi: number; pi: number }[] = [];
    for (let gi = 0; gi < pasti.length; gi++) {
      for (let pi = 0; pi < pasti[gi].length; pi++) {
        const chiave = chiaveNomePasto(pasti[gi][pi].nome);
        if (viste.has(chiave)) {
          daRigenerare.push({ gi, pi });
        } else {
          viste.add(chiave);
        }
      }
    }

    if (daRigenerare.length === 0) break;

    const nomiEsistenti = [...new Set(pasti.flat().map((p) => p.nome))];

    await Promise.all(
      daRigenerare.map(async ({ gi, pi }) => {
        try {
          pasti[gi][pi] = await regeneratePasto(profilo, giorni[gi].giorno, pasti[gi][pi], [], dispensa, nomiEsistenti);
        } catch (err) {
          console.error("assicuraVarieta regeneratePasto error:", err);
        }
      }),
    );
  }

  const giorniRigenerati = giorni.map((g, gi) => ({ giorno: g.giorno, pasti: pasti[gi] }));
  // I pasti appena rigenerati non sono ancora stati controllati per rischio
  // glutine: riusa validaGiorni (no-op se il profilo non ha quella
  // restrizione o se non c'era nessun doppione da sostituire).
  return validaGiorni(profilo, giorniRigenerati as Giorno[], dispensa);
}

/**
 * Valida sicurezza E varietà sul piano in ingresso, poi lo adatta entro il
 * budget se serve. Chiama internamente `assicuraVarieta` — sia sui giorni
 * iniziali sia dopo ogni tentativo di `adattaBudget` — così ogni chiamante
 * ottiene entrambi i controlli semplicemente passando da qui, senza dover
 * ricordarsi di invocare `assicuraVarieta` a parte (prima di questo, un
 * piano rivisto da `adattaBudget` per il costo poteva reintrodurre un
 * doppione senza che nessun controllo lo intercettasse).
 */
export async function adattaEntroBudget(
  profilo: ProfiloPerPiano,
  giorniIniziali: GiornoValidato[],
  supermercato: string | null,
  budget: number | null,
  dispensa: Map<string, number> = new Map(),
  fattoreCalibrazione: number = 1,
): Promise<{
  giorni: GiornoValidato[];
  groceryList: GroceryList;
  consumiDispensa: ConsumoDispensa[];
  budgetSuperato: boolean;
}> {
  let giorni = await assicuraVarieta(profilo, giorniIniziali, dispensa);
  let risultato = buildGroceryList(giorni, supermercato, dispensa, fattoreCalibrazione);

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
        dispensa,
      );
      const giorniValidati = await validaGiorni(profilo, pianoAdattato.giorni, dispensa);
      giorni = await assicuraVarieta(profilo, giorniValidati, dispensa);
      risultato = buildGroceryList(giorni, supermercato, dispensa, fattoreCalibrazione);
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
