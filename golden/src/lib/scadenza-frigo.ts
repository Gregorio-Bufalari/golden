// Stima una data di scadenza per una voce del Frigo, a partire dalla
// stessa classificazione di conservazione già usata per il testo
// descrittivo (conservazione.ts). Solo fresco e frigo_aperto hanno un
// limite "a breve" sensato (giorni, non mesi): per surgelati, dispensa
// secca o ingredienti non riconosciuti non si calcola nessuna scadenza,
// coerente col fatto che durano mesi.

import { categoriaConservazione, type Categoria, type ColoreScadenza } from "./conservazione";

// Limite superiore della fascia già mostrata in TESTO_PER_CATEGORIA
// (es. "fresco: consuma entro 2-4 giorni" -> stima sui 4 giorni): è una
// stima indicativa, non una data di scadenza certa stampata su una
// confezione reale.
const GIORNI_CONSERVAZIONE: Partial<Record<Categoria, number>> = {
  fresco: 4,
  frigo_aperto: 7,
};

/**
 * Data di scadenza stimata, a partire da quando l'ingrediente è entrato in
 * dispensa/frigo (`dataAcquisto`, "YYYY-MM-DD" — in pratica la `settimana`
 * della riga in `rimanenze`). Null se la categoria non ha una scadenza "a
 * breve" tracciabile (surgelato, dispensa, o ingrediente non riconosciuto).
 */
export function dataScadenzaStimata(nomeIngrediente: string, dataAcquisto: string): Date | null {
  const categoria = categoriaConservazione(nomeIngrediente);
  const giorni = categoria ? GIORNI_CONSERVAZIONE[categoria] : undefined;
  if (!giorni) return null;

  // Mezzanotte LOCALE (non UTC), stesso accorgimento di settimana.ts: evita
  // che l'aritmetica sui giorni scivoli di uno vicino al cambio di fuso.
  const acquisto = new Date(`${dataAcquisto}T00:00:00`);
  if (Number.isNaN(acquisto.getTime())) return null;

  const scadenza = new Date(acquisto);
  scadenza.setDate(acquisto.getDate() + giorni);
  return scadenza;
}

/** Giorni mancanti alla scadenza (negativo se già passata, 0 se scade oggi). */
export function giorniAllaScadenza(scadenza: Date, oggi: Date = new Date()): number {
  const mezzanotteOggi = new Date(oggi.getFullYear(), oggi.getMonth(), oggi.getDate());
  const mezzanotteScadenza = new Date(scadenza.getFullYear(), scadenza.getMonth(), scadenza.getDate());
  const msAlGiorno = 1000 * 60 * 60 * 24;
  return Math.round((mezzanotteScadenza.getTime() - mezzanotteOggi.getTime()) / msAlGiorno);
}

// Sotto questa soglia il pallino è rosso (urgente), sotto l'altra arancione
// (in avvicinamento) — oltre resta verde. Valori assoluti di giorni, non
// relativi alla fascia di conservazione: così lo stesso ingrediente passa
// da verde ad arancione a rosso avvicinandosi DAVVERO alla scadenza,
// invece di restare fisso allo stesso colore dal giorno dell'acquisto.
const SOGLIA_ROSSO_GIORNI = 1;
const SOGLIA_ARANCIONE_GIORNI = 3;

/**
 * Colore del pallino per il Frigo, in base ai giorni REALI rimanenti alla
 * scadenza stimata (null se la categoria non ne ha una "a breve"
 * tracciabile, es. surgelato/dispensa — resta verde, dura comunque mesi).
 */
export function coloreUrgenza(giorni: number | null): ColoreScadenza {
  if (giorni === null) return "verde";
  if (giorni <= SOGLIA_ROSSO_GIORNI) return "rosso";
  if (giorni <= SOGLIA_ARANCIONE_GIORNI) return "arancione";
  return "verde";
}
