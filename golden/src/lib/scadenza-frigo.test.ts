import { describe, it, expect } from "vitest";
import { dataScadenzaStimata, giorniAllaScadenza, coloreUrgenza } from "./scadenza-frigo";

describe("dataScadenzaStimata", () => {
  it("stima 4 giorni per un ingrediente fresco", () => {
    const scadenza = dataScadenzaStimata("Petto di pollo", "2026-10-05");
    expect(scadenza).toEqual(new Date(2026, 9, 9));
  });

  it("stima 7 giorni per un ingrediente da frigo aperto", () => {
    const scadenza = dataScadenzaStimata("Mozzarella", "2026-10-05");
    expect(scadenza).toEqual(new Date(2026, 9, 12));
  });

  it("restituisce null per un surgelato (dura mesi, nessuna scadenza a breve)", () => {
    expect(dataScadenzaStimata("Piselli surgelati", "2026-10-05")).toBeNull();
  });

  it("restituisce null per la dispensa secca", () => {
    expect(dataScadenzaStimata("Riso", "2026-10-05")).toBeNull();
  });

  it("restituisce null per un ingrediente non riconosciuto", () => {
    expect(dataScadenzaStimata("Qualcosa di sconosciuto", "2026-10-05")).toBeNull();
  });

  it("restituisce null per una data di acquisto non valida", () => {
    expect(dataScadenzaStimata("Pollo", "")).toBeNull();
  });

  it("attraversa correttamente il cambio di mese", () => {
    const scadenza = dataScadenzaStimata("Pollo", "2026-10-29");
    expect(scadenza).toEqual(new Date(2026, 10, 2));
  });
});

describe("giorniAllaScadenza", () => {
  it("restituisce 1 se la scadenza è domani", () => {
    const oggi = new Date(2026, 9, 5);
    const scadenza = new Date(2026, 9, 6);
    expect(giorniAllaScadenza(scadenza, oggi)).toBe(1);
  });

  it("restituisce 0 se scade oggi", () => {
    const oggi = new Date(2026, 9, 5);
    expect(giorniAllaScadenza(new Date(2026, 9, 5), oggi)).toBe(0);
  });

  it("restituisce un numero negativo se è già scaduta", () => {
    const oggi = new Date(2026, 9, 5);
    const scadenza = new Date(2026, 9, 3);
    expect(giorniAllaScadenza(scadenza, oggi)).toBe(-2);
  });

  it("ignora l'orario, confronta solo le date", () => {
    const oggi = new Date(2026, 9, 5, 23, 59);
    const scadenza = new Date(2026, 9, 6, 0, 1);
    expect(giorniAllaScadenza(scadenza, oggi)).toBe(1);
  });
});

describe("coloreUrgenza", () => {
  it("verde quando non c'è una scadenza a breve tracciabile (surgelato/dispensa)", () => {
    expect(coloreUrgenza(null)).toBe("verde");
  });

  it("rosso se scade oggi o domani", () => {
    expect(coloreUrgenza(0)).toBe("rosso");
    expect(coloreUrgenza(1)).toBe("rosso");
  });

  it("rosso anche se già scaduto", () => {
    expect(coloreUrgenza(-2)).toBe("rosso");
  });

  it("arancione se mancano 2-3 giorni", () => {
    expect(coloreUrgenza(2)).toBe("arancione");
    expect(coloreUrgenza(3)).toBe("arancione");
  });

  it("verde se mancano più di 3 giorni", () => {
    expect(coloreUrgenza(4)).toBe("verde");
  });

  it("cambia colore avvicinandosi alla scadenza dello stesso ingrediente, non resta fisso", () => {
    // Stesso ingrediente (es. un fresco con 4 giorni di margine): il colore
    // deve scalare verso il rosso man mano che i giorni rimanenti calano,
    // non restare identico dal giorno dell'acquisto alla scadenza.
    const sequenza = [4, 3, 2, 1, 0].map(coloreUrgenza);
    expect(sequenza).toEqual(["verde", "arancione", "arancione", "rosso", "rosso"]);
  });
});
