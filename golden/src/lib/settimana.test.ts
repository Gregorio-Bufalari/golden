import { describe, it, expect } from "vitest";
import { dataDelGiorno, etichettaGiorno, formattaData } from "./settimana";

describe("formattaData", () => {
  it("formatta una data come 'giorno mese'", () => {
    expect(formattaData(new Date(2026, 9, 7))).toBe("7 ottobre");
    expect(formattaData(new Date(2026, 0, 1))).toBe("1 gennaio");
  });
});

describe("dataDelGiorno", () => {
  it("calcola la data di ogni giorno della settimana a partire dal lunedì", () => {
    // 2026-10-05 è un lunedì.
    expect(dataDelGiorno("Lunedì", "2026-10-05")).toBe("5 ottobre");
    expect(dataDelGiorno("Martedì", "2026-10-05")).toBe("6 ottobre");
    expect(dataDelGiorno("Domenica", "2026-10-05")).toBe("11 ottobre");
  });

  it("attraversa correttamente il cambio di mese", () => {
    // 2026-10-26 è un lunedì; la domenica successiva cade a novembre.
    expect(dataDelGiorno("Domenica", "2026-10-26")).toBe("1 novembre");
  });

  it("restituisce null per un nome di giorno non riconosciuto", () => {
    expect(dataDelGiorno("Lundì", "2026-10-05")).toBeNull();
  });

  it("restituisce null per una settimana non valida", () => {
    expect(dataDelGiorno("Lunedì", "")).toBeNull();
    expect(dataDelGiorno("Lunedì", "non-una-data")).toBeNull();
  });
});

describe("etichettaGiorno", () => {
  it("combina giorno abbreviato e data abbreviata", () => {
    expect(etichettaGiorno("Mercoledì", "2026-10-05")).toBe("Mer 7 ott");
  });

  it("è stabile rispetto a quali pasti sono assegnati al giorno: dipende solo da nome giorno + settimana", () => {
    // Stesso input, indipendentemente da qualunque scambio di pasti avvenuto nel frattempo.
    expect(etichettaGiorno("Giovedì", "2026-10-05")).toBe(etichettaGiorno("Giovedì", "2026-10-05"));
  });

  it("usa la settimana passata, non quella corrente — serve per l'Archivio", () => {
    expect(etichettaGiorno("Lunedì", "2026-01-05")).toBe("Lun 5 gen");
  });

  it("torna al nome completo del giorno se la data non è calcolabile", () => {
    expect(etichettaGiorno("Lunedì", "")).toBe("Lunedì");
  });
});
