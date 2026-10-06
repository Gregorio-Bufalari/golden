import { describe, it, expect } from "vitest";
import { calcolaSprechiEvitati } from "./sprechi-evitati";

describe("calcolaSprechiEvitati", () => {
  it("restituisce zero senza check-in senza sprechi e senza nulla in Frigo", () => {
    const risultato = calcolaSprechiEvitati(0, []);
    expect(risultato.totale).toBe(0);
    expect(risultato.valoreFrigo).toBe(0);
    expect(risultato.valoreCheckin).toBe(0);
  });

  it("valorizza le settimane senza spreco dichiarato", () => {
    const risultato = calcolaSprechiEvitati(3, []);
    expect(risultato.valoreCheckin).toBe(24); // 3 x 8€
    expect(risultato.totale).toBe(24);
  });

  it("valorizza il contenuto del Frigo in base a peso e categoria di conservazione", () => {
    const risultato = calcolaSprechiEvitati(0, [
      { ingrediente: "Petto di pollo", unita: "g", quantita: 500 }, // fresco: 0.5kg x 4€
      { ingrediente: "Riso", unita: "kg", quantita: 1 }, // dispensa: 1kg x 2.5€
      { ingrediente: "Uova", unita: "pz", quantita: 4 }, // 4 x 1€
    ]);
    expect(risultato.valoreFrigo).toBeCloseTo(2 + 2.5 + 4, 5);
    expect(risultato.totale).toBeCloseTo(8.5, 5);
  });

  it("usa un valore di fallback per un ingrediente non riconosciuto", () => {
    const risultato = calcolaSprechiEvitati(0, [
      { ingrediente: "Qualcosa di sconosciuto", unita: "g", quantita: 1000 },
    ]);
    expect(risultato.valoreFrigo).toBe(3); // 1kg x 3€ (fallback)
  });

  it("combina il valore del Frigo e quello dei check-in nel totale", () => {
    const risultato = calcolaSprechiEvitati(2, [{ ingrediente: "Riso", unita: "kg", quantita: 2 }]);
    expect(risultato.valoreCheckin).toBe(16);
    expect(risultato.valoreFrigo).toBe(5);
    expect(risultato.totale).toBe(21);
  });
});
