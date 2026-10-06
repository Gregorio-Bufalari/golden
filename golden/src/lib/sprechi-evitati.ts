// Indicatore "Sprechi evitati": stima in euro, non una misura esatta —
// stesso spirito di altre stime già nell'app (confronto LARN, prezzo del
// piano). Combina due segnali già raccolti altrove, nessun dato nuovo da
// chiedere all'utente:
// - il valore di quello che è rimasto in Frigo/dispensa invece di essere
//   buttato (stessa classificazione di conservazione già mostrata lì);
// - le settimane in cui il check-in dichiara "nessuno spreco", valorizzate
//   con un costo medio di riferimento.

import { categoriaConservazione, type Categoria } from "./conservazione";

// Valore medio indicativo per kg/litro, per categoria di conservazione —
// non il prezzo reale di un ingrediente specifico (quello lo stima l'AI al
// momento della generazione del piano), solo un ordine di grandezza per
// dare un peso in euro al cibo salvato dallo spreco.
const VALORE_MEDIO_PER_KG: Record<Categoria | "non_riconosciuto", number> = {
  fresco: 4,
  frigo_aperto: 6,
  surgelato: 5,
  dispensa: 2.5,
  non_riconosciuto: 3,
};

// Pezzi/confezioni singole (es. uova) non si stimano bene a peso: una
// stima piatta per pezzo.
const VALORE_MEDIO_PER_PEZZO = 1;

// Costo medio indicativo di uno spreco alimentare evitato in una settimana
// — riferimento generico, non la spesa reale di quella settimana (già
// mostrata a parte nel confronto budget stimato/reale).
const COSTO_MEDIO_SPRECO_SETTIMANALE_EUR = 8;

export type VoceFrigo = { ingrediente: string; unita: string; quantita: number };

function valoreVoceFrigo(voce: VoceFrigo): number {
  if (voce.unita === "pz" || voce.unita === "confezione") {
    return voce.quantita * VALORE_MEDIO_PER_PEZZO;
  }
  const categoria = categoriaConservazione(voce.ingrediente) || "non_riconosciuto";
  const kgEquivalenti = voce.unita === "kg" || voce.unita === "l" ? voce.quantita : voce.quantita / 1000;
  return kgEquivalenti * VALORE_MEDIO_PER_KG[categoria];
}

export type SprechiEvitati = {
  totale: number;
  valoreFrigo: number;
  valoreCheckin: number;
};

/**
 * @param settimaneSenzaSprecoDichiarato Numero di check-in con `spreco: false`.
 * @param frigo Contenuto attuale di `rimanenze` (tab Frigo) per il profilo.
 */
export function calcolaSprechiEvitati(
  settimaneSenzaSprecoDichiarato: number,
  frigo: VoceFrigo[],
): SprechiEvitati {
  const valoreFrigo = frigo.reduce((sum, voce) => sum + valoreVoceFrigo(voce), 0);
  const valoreCheckin = settimaneSenzaSprecoDichiarato * COSTO_MEDIO_SPRECO_SETTIMANALE_EUR;

  return {
    totale: valoreFrigo + valoreCheckin,
    valoreFrigo,
    valoreCheckin,
  };
}
