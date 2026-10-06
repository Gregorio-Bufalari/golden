// Il piano salva solo il nome del giorno ("Lunedì", "Martedì"...) perché è
// quello usato come identificatore ovunque nel codice e nei prompt
// all'AI (es. "giovedì mangio fuori", lo scambio tra giorni). Questo file
// serve solo a derivare la data reale per MOSTRARLA accanto al nome,
// senza toccare quell'identificatore.

const GIORNI_SETTIMANA = [
  "Lunedì",
  "Martedì",
  "Mercoledì",
  "Giovedì",
  "Venerdì",
  "Sabato",
  "Domenica",
];

const MESI = [
  "gennaio",
  "febbraio",
  "marzo",
  "aprile",
  "maggio",
  "giugno",
  "luglio",
  "agosto",
  "settembre",
  "ottobre",
  "novembre",
  "dicembre",
];

/**
 * Data (giorno + mese, es. "6 ottobre") del giorno della settimana indicato,
 * dato il lunedì della settimana (`settimana`, "YYYY-MM-DD"). Null se il
 * nome del giorno o la data non sono riconosciuti. L'orario esplicito a
 * mezzanotte LOCALE (non UTC) evita che l'aritmetica sui giorni scivoli di
 * un giorno vicino al cambio di fuso orario tra il server (che genera
 * `settimana`) e il browser di chi legge.
 */
export function dataDelGiorno(nomeGiorno: string, settimana: string): string | null {
  const indice = GIORNI_SETTIMANA.indexOf(nomeGiorno);
  const lunedi = new Date(`${settimana}T00:00:00`);
  if (indice === -1 || Number.isNaN(lunedi.getTime())) return null;

  const data = new Date(lunedi);
  data.setDate(lunedi.getDate() + indice);
  return `${data.getDate()} ${MESI[data.getMonth()]}`;
}

/** Es. "Lunedì 6 ottobre". Se la data non si può calcolare, resta solo il nome del giorno. */
export function etichettaGiorno(nomeGiorno: string, settimana: string): string {
  const data = dataDelGiorno(nomeGiorno, settimana);
  return data ? `${nomeGiorno} ${data}` : nomeGiorno;
}
