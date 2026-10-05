import { describe, it, expect } from "vitest";
import { conservazioneTipica, categoriaConservazione, gruppoAcquisto } from "./conservazione";

describe("conservazioneTipica", () => {
  it("riconosce gli alimenti freschi deperibili", () => {
    expect(conservazioneTipica("Petto di pollo")).toMatch(/fresco deperibile/i);
    expect(conservazioneTipica("Pane")).toMatch(/fresco deperibile/i);
  });

  it("riconosce gli alimenti surgelati", () => {
    expect(conservazioneTipica("Spinaci surgelati")).toMatch(/surgelato/i);
    expect(conservazioneTipica("Piselli congelati")).toMatch(/surgelato/i);
  });

  it("riconosce i prodotti da frigo una volta aperti", () => {
    expect(conservazioneTipica("Mozzarella")).toMatch(/frigo, da aperto/i);
    expect(conservazioneTipica("Uova")).toMatch(/frigo, da aperto/i);
  });

  it("riconosce la dispensa secca", () => {
    expect(conservazioneTipica("Pasta")).toMatch(/dispensa secca/i);
    expect(conservazioneTipica("Riso")).toMatch(/dispensa secca/i);
  });

  it("lo stato indicato nel nome ha priorità sulla categoria base dell'ingrediente", () => {
    // Bug reale corretto: "merluzzo" matcherebbe "fresco", ma il nome dice
    // esplicitamente che è surgelato — deve vincere "surgelato".
    expect(conservazioneTipica("Filetti di merluzzo surgelati al naturale")).toMatch(/surgelato/i);

    // Bug reale corretto: "fagioli" matcherebbe "dispensa secca", ma sono
    // già lessati (cotti) — deve vincere "frigo, da aperto", non "mesi o anni".
    const esito = conservazioneTipica("Fagioli neri lessati");
    expect(esito).toMatch(/frigo, da aperto/i);
    expect(esito).not.toMatch(/dispensa secca/i);
  });

  it("il testo della dispensa secca non promette una durata assoluta", () => {
    // Una volta aperta una confezione non dura quanto una sigillata.
    expect(conservazioneTipica("Farina")).not.toMatch(/mesi o anni/i);
  });

  it("per un ingrediente non riconosciuto rimanda alla confezione", () => {
    expect(conservazioneTipica("Cumino senza glutine")).toMatch(/scadenza sulla confezione/i);
  });

  it("non è sensibile a maiuscole/minuscole", () => {
    expect(conservazioneTipica("POLLO")).toMatch(/fresco deperibile/i);
  });

  it("riconosce la frutta e verdura fresca, che non compare mai nel Frigo ma compare nella Spesa", () => {
    expect(conservazioneTipica("Pomodoro")).toMatch(/fresco deperibile/i);
    expect(conservazioneTipica("Zucchine")).toMatch(/fresco deperibile/i);
    expect(conservazioneTipica("Insalata mista")).toMatch(/fresco deperibile/i);
  });
});

describe("categoriaConservazione", () => {
  it("restituisce null per un ingrediente non riconosciuto", () => {
    expect(categoriaConservazione("Cumino senza glutine")).toBeNull();
  });

  it("restituisce la categoria grezza (non il testo) per un ingrediente riconosciuto", () => {
    expect(categoriaConservazione("Pollo")).toBe("fresco");
    expect(categoriaConservazione("Riso")).toBe("dispensa");
  });
});

describe("gruppoAcquisto", () => {
  it("mette i freschi deperibili e i prodotti da frigo aperto in 'subito'", () => {
    expect(gruppoAcquisto("Petto di pollo")).toBe("subito");
    expect(gruppoAcquisto("Pomodoro")).toBe("subito");
    expect(gruppoAcquisto("Mozzarella")).toBe("subito");
    expect(gruppoAcquisto("Uova")).toBe("subito");
  });

  it("mette surgelati e dispensa secca in 'puo_aspettare'", () => {
    expect(gruppoAcquisto("Spinaci surgelati")).toBe("puo_aspettare");
    expect(gruppoAcquisto("Pasta")).toBe("puo_aspettare");
    expect(gruppoAcquisto("Riso")).toBe("puo_aspettare");
  });

  it("un ingrediente non riconosciuto va in 'puo_aspettare' per difetto (di solito è una spezia da dispensa)", () => {
    expect(gruppoAcquisto("Cumino senza glutine")).toBe("puo_aspettare");
  });
});
