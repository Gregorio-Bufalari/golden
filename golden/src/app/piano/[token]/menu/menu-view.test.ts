import { describe, it, expect } from "vitest";
import { anteprimaPiatti, percentualeSuBase } from "./menu-view";

type GiornoInput = Parameters<typeof anteprimaPiatti>[0][number];

function giorno(nomePasti: string[]): GiornoInput {
  return {
    giorno: "Lunedì",
    pasti: nomePasti.map((nome) => ({
      tipo: "pranzo",
      nome,
      ingredienti: [],
      tempo_preparazione_min: 20,
      nutrizione: { calorie: 0, proteine_g: 0, carboidrati_g: 0, grassi_g: 0, fibre_g: 0 },
    })),
  };
}

describe("anteprimaPiatti", () => {
  it("elenca tutti i piatti quando sono due o meno", () => {
    const giorni = [giorno(["Pollo al forno", "Pasta al pesto"])];
    expect(anteprimaPiatti(giorni)).toBe("Pollo al forno, Pasta al pesto");
  });

  it("mostra solo i primi due piatti e il conteggio dei restanti", () => {
    const giorni = [giorno(["A", "B", "C"]), giorno(["D"])];
    expect(anteprimaPiatti(giorni)).toBe("A, B +2 altri");
  });
});

describe("percentualeSuBase", () => {
  it("restituisce una stringa vuota quando il totale coincide con la base", () => {
    expect(percentualeSuBase(80, 80)).toBe("");
  });

  it("mostra una percentuale negativa con il segno quando il totale è più basso", () => {
    expect(percentualeSuBase(64, 80)).toBe("-20%");
  });

  it("mostra una percentuale positiva con il segno quando il totale è più alto", () => {
    expect(percentualeSuBase(96, 80)).toBe("+20%");
  });

  it("restituisce una stringa vuota se la base non è positiva", () => {
    expect(percentualeSuBase(50, 0)).toBe("");
  });
});
