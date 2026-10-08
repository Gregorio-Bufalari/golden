// Cosa notificare per le scadenze del Frigo — condiviso tra il banner
// in-app di fallback (frigo/page.tsx) e l'invio delle notifiche push vere
// (/api/push/notifica-scadenze), così i due canali concordano sempre su
// cosa conta come "in scadenza".

import { dataScadenzaStimata, giorniAllaScadenza } from "./scadenza-frigo";

export type VoceRimanenza = {
  ingrediente: string;
  settimana: string;
};

/**
 * Ingredienti che scadono esattamente domani, a partire dalle righe della
 * dispensa (stessa stima usata per il banner Frigo e per il pallino
 * colorato). Segnale naturalmente "una tantum" per ogni ingrediente —
 * domani non lo è più il giorno dopo — quindi la notifica push può usarlo
 * direttamente senza dover tracciare "già notificato" da qualche parte:
 * a differenza di "già scaduto" (che resterebbe vero per giorni), non
 * rischia di notificare lo stesso ingrediente più di una volta.
 */
export function ingredientiInScadenzaDomani(voci: VoceRimanenza[], oggi: Date = new Date()): string[] {
  return voci
    .filter((v) => {
      const scadenza = dataScadenzaStimata(v.ingrediente, v.settimana);
      return scadenza !== null && giorniAllaScadenza(scadenza, oggi) === 1;
    })
    .map((v) => v.ingrediente);
}

/** Testo della notifica push, stessa formulazione del banner in-app. */
export function testoNotificaScadenza(ingredienti: string[]): { titolo: string; corpo: string } {
  return {
    titolo: "In scadenza domani",
    corpo: `${ingredienti.join(", ")}: usali presto per non sprecarli.`,
  };
}
