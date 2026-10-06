// Controllo leggero (nessuna AI) eseguito interamente sui dati del piano
// già caricati lato client, prima di applicare uno scambio pasti tra
// giorni: segnala se il giorno risultante avrebbe lo stesso ingrediente
// "principale" sia a pranzo che a cena, invece di applicare lo scambio in
// silenzio.

export type IngredienteConReparto = { nome: string; reparto: string };
export type PastoConIngredienti = { ingredienti: IngredienteConReparto[] };
export type GiornoConPasti = { giorno: string; pasti: PastoConIngredienti[] };

// "Principale" = proteina del piatto: condimenti/spezie/basi da dispensa si
// riusano legittimamente in più pasti (è anzi incoraggiato altrove
// nell'app), frutta e verdura ripetuta nello stesso giorno è normale — il
// caso davvero poco vario è la stessa carne/pesce/uovo/formaggio sia a
// pranzo che a cena.
const REPARTI_PRINCIPALI = new Set(["Carne e pesce", "Latticini e uova"]);

/** Ingredienti "principali" (nome originale) condivisi tra i due pasti di un giorno. */
export function ingredientiPrincipaliRipetuti(pasti: PastoConIngredienti[]): string[] {
  if (pasti.length !== 2) return [];
  const [primo, secondo] = pasti;

  const nomiPrimo = new Set(
    primo.ingredienti
      .filter((i) => REPARTI_PRINCIPALI.has(i.reparto))
      .map((i) => i.nome.trim().toLowerCase()),
  );

  const ripetuti = secondo.ingredienti.filter(
    (i) => REPARTI_PRINCIPALI.has(i.reparto) && nomiPrimo.has(i.nome.trim().toLowerCase()),
  );

  return [...new Set(ripetuti.map((i) => i.nome))];
}

export type ConflittoGiorno = { giorno: string; ingredienti: string[] };

/**
 * Simula lo scambio (senza applicarlo) e restituisce i giorni risultanti
 * che avrebbero una ripetizione problematica. Array vuoto se lo scambio è
 * pulito.
 */
export function conflittiDopoScambio(
  giorni: GiornoConPasti[],
  giornoA: string,
  indiceA: number,
  giornoB: string,
  indiceB: number,
): ConflittoGiorno[] {
  const recordA = giorni.find((g) => g.giorno === giornoA);
  const recordB = giorni.find((g) => g.giorno === giornoB);
  if (!recordA || !recordB) return [];

  const pastiA = [...recordA.pasti];
  const pastiB = [...recordB.pasti];
  const pastoA = pastiA[indiceA];
  const pastoB = pastiB[indiceB];
  if (!pastoA || !pastoB) return [];

  pastiA[indiceA] = pastoB;
  pastiB[indiceB] = pastoA;

  const conflitti: ConflittoGiorno[] = [];
  const ripetutiA = ingredientiPrincipaliRipetuti(pastiA);
  if (ripetutiA.length > 0) conflitti.push({ giorno: giornoA, ingredienti: ripetutiA });

  if (giornoB !== giornoA) {
    const ripetutiB = ingredientiPrincipaliRipetuti(pastiB);
    if (ripetutiB.length > 0) conflitti.push({ giorno: giornoB, ingredienti: ripetutiB });
  }

  return conflitti;
}

/**
 * Cerca un altro giorno (stesso indice pasto, es. pranzo con pranzo) con
 * cui scambiare `giornoA`/`indiceA` senza generare conflitti — per
 * proporre un'alternativa invece di limitarsi a bloccare lo scambio.
 */
export function suggerisciGiornoAlternativo(
  giorni: GiornoConPasti[],
  giornoA: string,
  indiceA: number,
  giornoDaEvitare: string,
  indiceB: number,
): string | null {
  for (const candidato of giorni) {
    if (candidato.giorno === giornoA || candidato.giorno === giornoDaEvitare) continue;
    if (!candidato.pasti[indiceB]) continue;
    if (conflittiDopoScambio(giorni, giornoA, indiceA, candidato.giorno, indiceB).length === 0) {
      return candidato.giorno;
    }
  }
  return null;
}
