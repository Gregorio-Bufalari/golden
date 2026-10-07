import { describe, it, expect } from "vitest";
import { fattoreCalibrazione, type CheckinPerCalibrazione } from "./calibrazione-prezzi";

function checkin(overrides: Partial<CheckinPerCalibrazione>): CheckinPerCalibrazione {
  return { retailer_usato: "Conad", spesa_reale: 50, budget_stimato: 50, ...overrides };
}

describe("fattoreCalibrazione", () => {
  it("restituisce 1 se non c'è un retailer di riferimento", () => {
    expect(fattoreCalibrazione([checkin({})], null)).toBe(1);
  });

  it("restituisce 1 se ci sono meno di 2 check-in per quel retailer (campione troppo piccolo)", () => {
    const checkins = [checkin({ spesa_reale: 70, budget_stimato: 50 })];
    expect(fattoreCalibrazione(checkins, "Conad")).toBe(1);
  });

  it("calcola la media del rapporto spesa_reale/budget_stimato con almeno 2 check-in", () => {
    const checkins = [
      checkin({ spesa_reale: 60, budget_stimato: 50 }), // 1.2
      checkin({ spesa_reale: 55, budget_stimato: 50 }), // 1.1
    ];
    expect(fattoreCalibrazione(checkins, "Conad")).toBeCloseTo(1.15, 5);
  });

  it("ignora i check-in con un retailer diverso da quello richiesto", () => {
    const checkins = [
      checkin({ retailer_usato: "Conad", spesa_reale: 60, budget_stimato: 50 }),
      checkin({ retailer_usato: "Conad", spesa_reale: 55, budget_stimato: 50 }),
      checkin({ retailer_usato: "Esselunga", spesa_reale: 200, budget_stimato: 50 }), // non deve influenzare Conad
    ];
    expect(fattoreCalibrazione(checkins, "Conad")).toBeCloseTo(1.15, 5);
  });

  it("ignora i check-in senza spesa_reale o budget_stimato", () => {
    const checkins = [
      checkin({ spesa_reale: 60, budget_stimato: 50 }),
      checkin({ spesa_reale: null, budget_stimato: 50 }),
      checkin({ spesa_reale: 55, budget_stimato: null }),
    ];
    // Un solo check-in valido -> sotto la soglia minima -> nessuna correzione.
    expect(fattoreCalibrazione(checkins, "Conad")).toBe(1);
  });

  it("limita il fattore al minimo 0.7 e al massimo 1.4", () => {
    const moltoBasso = [
      checkin({ spesa_reale: 10, budget_stimato: 50 }),
      checkin({ spesa_reale: 10, budget_stimato: 50 }),
    ];
    const moltoAlto = [
      checkin({ spesa_reale: 150, budget_stimato: 50 }),
      checkin({ spesa_reale: 150, budget_stimato: 50 }),
    ];
    expect(fattoreCalibrazione(moltoBasso, "Conad")).toBe(0.7);
    expect(fattoreCalibrazione(moltoAlto, "Conad")).toBe(1.4);
  });
});
