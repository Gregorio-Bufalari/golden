import { describe, it, expect } from "vitest";
import {
  ingredientiPrincipaliRipetuti,
  conflittiDopoScambio,
  suggerisciGiornoAlternativo,
  type GiornoConPasti,
} from "./varieta-giorno";

function ing(nome: string, reparto: string) {
  return { nome, reparto };
}

function pasto(ingredienti: ReturnType<typeof ing>[]) {
  return { ingredienti };
}

describe("ingredientiPrincipaliRipetuti", () => {
  it("segnala lo stesso ingrediente principale a pranzo e cena", () => {
    const pasti = [
      pasto([ing("Pollo", "Carne e pesce"), ing("Olio", "Dispensa")]),
      pasto([ing("Pollo", "Carne e pesce"), ing("Insalata", "Frutta e verdura")]),
    ];
    expect(ingredientiPrincipaliRipetuti(pasti)).toEqual(["Pollo"]);
  });

  it("non segnala ingredienti da dispensa (condimenti) ripetuti", () => {
    const pasti = [pasto([ing("Olio", "Dispensa")]), pasto([ing("Olio", "Dispensa")])];
    expect(ingredientiPrincipaliRipetuti(pasti)).toEqual([]);
  });

  it("non segnala frutta/verdura ripetuta", () => {
    const pasti = [pasto([ing("Pomodoro", "Frutta e verdura")]), pasto([ing("Pomodoro", "Frutta e verdura")])];
    expect(ingredientiPrincipaliRipetuti(pasti)).toEqual([]);
  });

  it("non segnala nulla se i due pasti non condividono ingredienti principali", () => {
    const pasti = [pasto([ing("Pollo", "Carne e pesce")]), pasto([ing("Uova", "Latticini e uova")])];
    expect(ingredientiPrincipaliRipetuti(pasti)).toEqual([]);
  });

  it("confronta i nomi senza distinguere maiuscole/minuscole", () => {
    const pasti = [pasto([ing("pollo", "Carne e pesce")]), pasto([ing("Pollo", "Carne e pesce")])];
    expect(ingredientiPrincipaliRipetuti(pasti)).toEqual(["Pollo"]);
  });

  it("con 3 pasti al giorno (colazione+pranzo+cena) segnala un principale ripetuto tra colazione e cena", () => {
    // Bug reale corretto in questa feature: la versione precedente aveva
    // `if (pasti.length !== 2) return [];`, quindi con 3 pasti al giorno
    // (dopo l'aggiunta della colazione) questo controllo smetteva
    // silenziosamente di funzionare per qualunque giorno.
    const pasti = [
      pasto([ing("Uova", "Latticini e uova")]),
      pasto([ing("Pasta", "Pane e cereali")]),
      pasto([ing("Uova", "Latticini e uova")]),
    ];
    expect(ingredientiPrincipaliRipetuti(pasti)).toEqual(["Uova"]);
  });

  it("con 3 pasti al giorno non segnala nulla se nessun principale si ripete", () => {
    const pasti = [
      pasto([ing("Yogurt", "Latticini e uova")]),
      pasto([ing("Pollo", "Carne e pesce")]),
      pasto([ing("Pesce", "Carne e pesce")]),
    ];
    expect(ingredientiPrincipaliRipetuti(pasti)).toEqual([]);
  });

  it("non conta due volte un principale usato più volte nello stesso pasto", () => {
    const pasti = [
      pasto([ing("Uova", "Latticini e uova"), ing("Uova", "Latticini e uova")]),
      pasto([ing("Pasta", "Pane e cereali")]),
    ];
    expect(ingredientiPrincipaliRipetuti(pasti)).toEqual([]);
  });
});

describe("conflittiDopoScambio", () => {
  const giorni: GiornoConPasti[] = [
    {
      giorno: "Lunedì",
      pasti: [pasto([ing("Pasta", "Pane e cereali")]), pasto([ing("Pollo", "Carne e pesce")])],
    },
    {
      giorno: "Martedì",
      pasti: [pasto([ing("Pollo", "Carne e pesce")]), pasto([ing("Riso", "Dispensa")])],
    },
    {
      giorno: "Mercoledì",
      pasti: [pasto([ing("Pesce", "Carne e pesce")]), pasto([ing("Uova", "Latticini e uova")])],
    },
  ];

  it("non segnala nulla quando lo scambio non introduce doppioni (Pollo per Pollo)", () => {
    // Cena di Lunedì (Pollo) <-> pranzo di Martedì (Pollo): risultato
    // equivalente a prima, nessun nuovo doppione in nessuno dei due giorni.
    const conflitti = conflittiDopoScambio(giorni, "Lunedì", 1, "Martedì", 0);
    expect(conflitti).toEqual([]);
  });

  it("rileva un conflitto reale introdotto dallo scambio", () => {
    // Pranzo di Lunedì (Pasta) <-> pranzo di Martedì (Pollo): Lunedì
    // finisce con Pollo sia a pranzo (arrivato da Martedì) sia a cena
    // (originale) — un doppione che prima non c'era.
    const conflitti = conflittiDopoScambio(giorni, "Lunedì", 0, "Martedì", 0);
    expect(conflitti).toEqual([{ giorno: "Lunedì", ingredienti: ["Pollo"] }]);
  });

  it("restituisce array vuoto per giorni o indici inesistenti", () => {
    expect(conflittiDopoScambio(giorni, "Lunedì", 0, "Venerdì", 0)).toEqual([]);
    expect(conflittiDopoScambio(giorni, "Lunedì", 5, "Martedì", 0)).toEqual([]);
  });

  it("rileva un conflitto sullo scambio della colazione (indice 0 con 3 pasti al giorno)", () => {
    const giorniConColazione: GiornoConPasti[] = [
      {
        giorno: "Lunedì",
        pasti: [
          pasto([ing("Uova", "Latticini e uova")]),
          pasto([ing("Pasta", "Pane e cereali")]),
          pasto([ing("Pollo", "Carne e pesce")]),
        ],
      },
      {
        giorno: "Martedì",
        pasti: [
          pasto([ing("Yogurt", "Latticini e uova")]),
          pasto([ing("Riso", "Dispensa")]),
          pasto([ing("Uova", "Latticini e uova")]),
        ],
      },
    ];
    // Colazione di Lunedì (Uova) <-> colazione di Martedì (Yogurt): Martedì
    // finisce con Uova sia a colazione (arrivata da Lunedì) sia a cena.
    const conflitti = conflittiDopoScambio(giorniConColazione, "Lunedì", 0, "Martedì", 0);
    expect(conflitti).toEqual([{ giorno: "Martedì", ingredienti: ["Uova"] }]);
  });
});

describe("suggerisciGiornoAlternativo", () => {
  const giorni: GiornoConPasti[] = [
    {
      giorno: "Lunedì",
      pasti: [pasto([ing("Pasta", "Pane e cereali")]), pasto([ing("Pollo", "Carne e pesce")])],
    },
    {
      giorno: "Martedì",
      pasti: [pasto([ing("Pollo", "Carne e pesce")]), pasto([ing("Riso", "Dispensa")])],
    },
    {
      giorno: "Mercoledì",
      pasti: [pasto([ing("Pesce", "Carne e pesce")]), pasto([ing("Uova", "Latticini e uova")])],
    },
  ];

  it("propone un giorno alternativo senza conflitti quando quello scelto ne genera uno", () => {
    // Lunedì pranzo (Pasta) <-> Martedì pranzo (Pollo) genera un conflitto
    // su Lunedì (Pollo a pranzo e cena). Mercoledì pranzo (Pesce) invece va bene.
    const alternativa = suggerisciGiornoAlternativo(giorni, "Lunedì", 0, "Martedì", 0);
    expect(alternativa).toBe("Mercoledì");
  });

  it("restituisce null se nessun altro giorno è disponibile senza conflitti", () => {
    const soloDue = giorni.slice(0, 2);
    const alternativa = suggerisciGiornoAlternativo(soloDue, "Lunedì", 0, "Martedì", 0);
    expect(alternativa).toBeNull();
  });
});
