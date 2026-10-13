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

const MESI_ABBREV = ["gen", "feb", "mar", "apr", "mag", "giu", "lug", "ago", "set", "ott", "nov", "dic"];

const GIORNI_ABBREV: Record<string, string> = {
  Lunedì: "Lun",
  Martedì: "Mar",
  Mercoledì: "Mer",
  Giovedì: "Gio",
  Venerdì: "Ven",
  Sabato: "Sab",
  Domenica: "Dom",
};

/** Es. "6 ottobre". */
export function formattaData(data: Date): string {
  return `${data.getDate()} ${MESI[data.getMonth()]}`;
}

/** Es. "6 ott". */
function formattaDataBreve(data: Date): string {
  return `${data.getDate()} ${MESI_ABBREV[data.getMonth()]}`;
}

/**
 * Data reale del giorno della settimana indicato, dato il lunedì della
 * settimana (`settimana`, "YYYY-MM-DD") — questa è la chiave stabile di un
 * giorno: resta la stessa indipendentemente da quali pasti gli sono
 * assegnati (uno scambio sposta solo i pasti, mai questa data) e da quando
 * la pagina viene aperta (va sempre presa dalla `settimana` della riga
 * `weekly_plans` in questione, mai da "oggi" — così una settimana
 * dell'Archivio mostra le sue date reali, non quelle della settimana
 * corrente). Null se il nome del giorno o la data non sono riconosciuti.
 * L'orario esplicito a mezzanotte LOCALE (non UTC) evita che l'aritmetica
 * sui giorni scivoli di un giorno vicino al cambio di fuso orario tra il
 * server (che genera `settimana`) e il browser di chi legge.
 */
function dataOggettoDelGiorno(nomeGiorno: string, settimana: string): Date | null {
  const indice = GIORNI_SETTIMANA.indexOf(nomeGiorno);
  const lunedi = new Date(`${settimana}T00:00:00`);
  if (indice === -1 || Number.isNaN(lunedi.getTime())) return null;

  const data = new Date(lunedi);
  data.setDate(lunedi.getDate() + indice);
  return data;
}

/** Es. "6 ottobre". Vedi `dataOggettoDelGiorno` per la semantica della data. */
export function dataDelGiorno(nomeGiorno: string, settimana: string): string | null {
  const data = dataOggettoDelGiorno(nomeGiorno, settimana);
  return data ? formattaData(data) : null;
}

/**
 * Es. "Gio 6 ott" — l'identificatore mostrato in UI per un giorno del
 * Menu: giorno abbreviato + data reale abbreviata, non un indice di
 * posizione. Se la data non si può calcolare, resta solo il nome completo
 * del giorno (meglio un'etichetta chiara che una sigla senza contesto).
 */
export function etichettaGiorno(nomeGiorno: string, settimana: string): string {
  const data = dataOggettoDelGiorno(nomeGiorno, settimana);
  if (!data) return nomeGiorno;
  const abbrev = GIORNI_ABBREV[nomeGiorno] ?? nomeGiorno;
  return `${abbrev} ${formattaDataBreve(data)}`;
}
