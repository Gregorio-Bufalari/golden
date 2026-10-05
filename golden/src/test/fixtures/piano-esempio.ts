import type { Giorno, Ingrediente, Nutrizione, Pasto } from "@/lib/claude";

// Piano di esempio fisso, riusato da tutti i test che hanno bisogno di un
// piano settimanale completo (7 giorni x pranzo/cena = 14 pasti). Pensato
// per esercitare in un colpo solo i casi che contano:
// - "Pasta al pomodoro" (Lunedì pranzo): ingrediente a rischio glutine
//   ("pasta"), per i test di glutine-check.ts / validaGiorni.
// - "Petto di pollo alla griglia" (Lunedì cena): 300g di pollo, confezione
//   reale da 500g -> 200g di avanzo, lo stesso scenario verificato con
//   l'utente durante lo sviluppo della dispensa.
// - ingredienti freschi (es. insalata, pomodori) mai arrotondati a
//   confezione, per verificare che quel comportamento resti tale.

export function nutrizione(overrides: Partial<Nutrizione> = {}): Nutrizione {
  return {
    calorie: 500,
    proteine_g: 25,
    carboidrati_g: 60,
    grassi_g: 15,
    fibre_g: 5,
    ...overrides,
  };
}

export function ingrediente(overrides: Partial<Ingrediente>): Ingrediente {
  return {
    nome: "Ingrediente generico",
    quantita: 100,
    unita: "g",
    reparto: "Dispensa",
    prezzo_stimato_eur: 1,
    ...overrides,
  };
}

export function pasto(overrides: Partial<Pasto> & Pick<Pasto, "tipo" | "nome" | "ingredienti">): Pasto {
  return {
    tempo_preparazione_min: 20,
    nutrizione: nutrizione(),
    preparazione: ["Prepara gli ingredienti.", "Cuoci e servi."],
    ...overrides,
  };
}

const GIORNI_SETTIMANA = [
  "Lunedì",
  "Martedì",
  "Mercoledì",
  "Giovedì",
  "Venerdì",
  "Sabato",
  "Domenica",
] as const;

/** Un piano completo (7 giorni x 2 pasti), identico a ogni chiamata. */
export function creaPianoEsempio(): Giorno[] {
  return GIORNI_SETTIMANA.map((giorno, i) => {
    if (i === 0) {
      // Lunedì: i due pasti "speciali" descritti sopra.
      return {
        giorno,
        pasti: [
          pasto({
            tipo: "pranzo",
            nome: "Pasta al pomodoro",
            ingredienti: [
              ingrediente({
                nome: "Pasta",
                quantita: 100,
                unita: "g",
                reparto: "Pane e cereali",
                prezzo_stimato_eur: 0.3,
              }),
              ingrediente({
                nome: "Pomodoro",
                quantita: 200,
                unita: "g",
                reparto: "Frutta e verdura",
                prezzo_stimato_eur: 0.6,
              }),
              ingrediente({
                nome: "Olio extravergine",
                quantita: 10,
                unita: "ml",
                reparto: "Dispensa",
                prezzo_stimato_eur: 0.15,
              }),
            ],
          }),
          pasto({
            tipo: "cena",
            nome: "Petto di pollo alla griglia",
            ingredienti: [
              ingrediente({
                nome: "Petto di pollo",
                quantita: 300,
                unita: "g",
                reparto: "Carne e pesce",
                prezzo_stimato_eur: 3.3,
              }),
              ingrediente({
                nome: "Insalata mista",
                quantita: 150,
                unita: "g",
                reparto: "Frutta e verdura",
                prezzo_stimato_eur: 0.8,
              }),
            ],
          }),
        ],
      };
    }

    // Gli altri sei giorni: pasti semplici e vari, nessun caso speciale.
    return {
      giorno,
      pasti: [
        pasto({
          tipo: "pranzo",
          nome: `Riso con verdure (${giorno})`,
          ingredienti: [
            ingrediente({
              nome: "Riso",
              quantita: 80,
              unita: "g",
              reparto: "Dispensa",
              prezzo_stimato_eur: 0.2,
            }),
            ingrediente({
              nome: "Zucchine",
              quantita: 150,
              unita: "g",
              reparto: "Frutta e verdura",
              prezzo_stimato_eur: 0.5,
            }),
          ],
        }),
        pasto({
          tipo: "cena",
          nome: `Uova e verdure (${giorno})`,
          ingredienti: [
            ingrediente({
              nome: "Uova",
              quantita: 2,
              unita: "pz",
              reparto: "Latticini e uova",
              prezzo_stimato_eur: 0.6,
            }),
            ingrediente({
              nome: "Spinaci surgelati",
              quantita: 100,
              unita: "g",
              reparto: "Surgelati",
              prezzo_stimato_eur: 0.4,
            }),
          ],
        }),
      ],
    };
  });
}
