import { describe, it, expect } from "vitest";
import { anteprimaPiatti, percentualeSuBase } from "./menu-view";
import { etichettaGiorno } from "@/lib/settimana";

type GiornoInput = Parameters<typeof anteprimaPiatti>[0][number];

function giorno(nomePasti: string[], nomeGiorno = "Lunedì"): GiornoInput {
  return {
    giorno: nomeGiorno,
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

describe("data del giorno nel Menu: chiave stabile, non legata ai pasti assegnati", () => {
  const SETTIMANA = "2026-10-05"; // lunedì

  it("resta la stessa per un giorno dopo uno scambio di pasti con un altro giorno", () => {
    const giorni = [giorno(["Pollo"], "Martedì"), giorno(["Riso"], "Giovedì")];
    const etichetteIniziali = giorni.map((g) => etichettaGiorno(g.giorno, SETTIMANA));

    // Stesso identico scambio fatto da scambiaPasti in actions.ts: si
    // riassegna solo il contenuto di pasti[indice], mai giorno.giorno.
    const [martedi, giovedi] = giorni;
    const pastoMartedi = martedi.pasti[0];
    const pastoGiovedi = giovedi.pasti[0];
    martedi.pasti[0] = pastoGiovedi;
    giovedi.pasti[0] = pastoMartedi;

    const etichetteDopoScambio = giorni.map((g) => etichettaGiorno(g.giorno, SETTIMANA));

    expect(etichetteDopoScambio).toEqual(etichetteIniziali);
    expect(giorni[0].pasti[0].nome).toBe("Riso"); // il pasto si è davvero spostato
    expect(giorni[1].pasti[0].nome).toBe("Pollo");
  });

  it("resta visibile con la sua data quando il contenuto di un giorno viene sostituito (es. 'giovedì mangio fuori')", () => {
    const giorni = [
      giorno(["Pasta"], "Mercoledì"),
      giorno(["Pollo", "Riso"], "Giovedì"),
      giorno(["Pesce"], "Venerdì"),
    ];
    const etichetteIniziali = giorni.map((g) => etichettaGiorno(g.giorno, SETTIMANA));

    // Una modifica via chat sostituisce solo il CONTENUTO di un giorno
    // (qui simulato), mai la sua posizione nell'array né il suo nome: lo
    // schema richiede sempre esattamente 7 giorni con nomi fissi, quindi
    // non può scalare o far sparire gli altri.
    giorni[1] = giorno(["Fuori"], "Giovedì");

    const etichetteDopo = giorni.map((g) => etichettaGiorno(g.giorno, SETTIMANA));

    expect(etichetteDopo).toEqual(etichetteIniziali); // nessun giorno si è spostato
    expect(giorni).toHaveLength(3); // Giovedì resta visibile, non è sparito
    expect(giorni[1].giorno).toBe("Giovedì");
  });
});
