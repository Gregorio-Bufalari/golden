import { describe, it, expect } from "vitest";
import { ingredientiInScadenzaDomani, testoNotificaScadenza } from "./notifiche-scadenza";

const OGGI = new Date(2026, 9, 5);

describe("ingredientiInScadenzaDomani", () => {
  it("include un ingrediente fresco che scade esattamente domani", () => {
    // Fresco: 4 giorni dall'acquisto. Acquistato il 2 ottobre -> scade il 6.
    const voci = [{ ingrediente: "Petto di pollo", settimana: "2026-10-02" }];
    expect(ingredientiInScadenzaDomani(voci, OGGI)).toEqual(["Petto di pollo"]);
  });

  it("esclude un ingrediente già scaduto (non solo \"domani\")", () => {
    const voci = [{ ingrediente: "Petto di pollo", settimana: "2026-09-25" }];
    expect(ingredientiInScadenzaDomani(voci, OGGI)).toEqual([]);
  });

  it("esclude un ingrediente che scade tra più di un giorno", () => {
    const voci = [{ ingrediente: "Petto di pollo", settimana: "2026-10-04" }];
    expect(ingredientiInScadenzaDomani(voci, OGGI)).toEqual([]);
  });

  it("esclude un ingrediente senza scadenza a breve (es. surgelato)", () => {
    const voci = [{ ingrediente: "Piselli surgelati", settimana: "2026-10-02" }];
    expect(ingredientiInScadenzaDomani(voci, OGGI)).toEqual([]);
  });

  it("elenca più ingredienti se più d'uno scade domani", () => {
    const voci = [
      { ingrediente: "Petto di pollo", settimana: "2026-10-02" },
      { ingrediente: "Mozzarella", settimana: "2026-09-29" },
    ];
    expect(ingredientiInScadenzaDomani(voci, OGGI)).toEqual(["Petto di pollo", "Mozzarella"]);
  });
});

describe("testoNotificaScadenza", () => {
  it("elenca gli ingredienti nel corpo del messaggio", () => {
    const { titolo, corpo } = testoNotificaScadenza(["Latte", "Pollo"]);
    expect(titolo).toBe("In scadenza domani");
    expect(corpo).toContain("Latte, Pollo");
  });
});
