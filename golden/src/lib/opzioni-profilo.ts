export const RESTRIZIONI_OPTIONS = [
  "Glutine (celiachia)",
  "Lattosio",
  "Vegetariano",
  "Vegano",
  "Frutta a guscio",
  "Uova",
  "Pesce/Crostacei",
  "Nessuna restrizione",
];

export const OBIETTIVO_OPTIONS = [
  "Mangiare meglio",
  "Risparmiare",
  "Ridurre gli sprechi",
  "Ridurre le decisioni",
  "Gestire il peso",
];

export const CUCINA_OPTIONS = ["Italiana", "Mediterranea", "Asiatica", "Messicana", "Altro"];

export const PERSONE_OPTIONS: { value: string; label: string }[] = [
  { value: "1", label: "1 persona" },
  { value: "2", label: "2 persone" },
  { value: "3", label: "3 persone" },
  { value: "4", label: "4 persone" },
  { value: "5", label: "5 persone" },
  { value: "6", label: "6 o più persone" },
];

export const TEMPO_OPTIONS = [
  { value: 15, label: "Fino a 15 minuti" },
  { value: 30, label: "Fino a 30 minuti" },
  { value: 45, label: "Fino a 45 minuti" },
  { value: 60, label: "60 minuti o più" },
];

export const SUPERMERCATO_OPTIONS = [
  "Esselunga",
  "Coop",
  "Conad",
  "Carrefour",
  "Lidl",
  "Eurospin",
  "Altro",
];

export const LIVELLO_ATTIVITA_OPTIONS: { value: "sedentario" | "moderato" | "attivo"; label: string }[] = [
  { value: "sedentario", label: "Sedentario" },
  { value: "moderato", label: "Moderato" },
  { value: "attivo", label: "Attivo" },
];
