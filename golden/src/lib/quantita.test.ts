import { describe, it, expect } from "vitest";
import { formattaQuantita } from "./quantita";

describe("formattaQuantita", () => {
  it("converte grammi in kg oltre i 1000g", () => {
    expect(formattaQuantita(1000, "g")).toBe("1 kg");
    expect(formattaQuantita(1500, "g")).toBe("1.5 kg");
  });

  it("converte millilitri in litri oltre i 1000ml", () => {
    expect(formattaQuantita(1000, "ml")).toBe("1 l");
    expect(formattaQuantita(750, "ml")).toBe("750 ml");
  });

  it("arrotonda a una cifra decimale per le altre quantità", () => {
    expect(formattaQuantita(0.666, "pz")).toBe("0.7 pz");
    expect(formattaQuantita(2, "pz")).toBe("2 pz");
  });
});
