// Confronto indicativo tra l'apporto nutrizionale settimanale del piano e i
// valori di riferimento LARN (Livelli di Assunzione di Riferimento di
// Nutrienti) per il profilo. Calcolo interamente deterministico (formule
// standard + tabelle statiche), MAI chiesto all'AI — solo una stima
// generale, non un parere medico/nutrizionale personalizzato.

export type DatiBiometrici = {
  sesso: "M" | "F";
  eta: number;
  peso_kg: number;
  altezza_cm: number;
  livello_attivita: "sedentario" | "moderato" | "attivo";
};

export type TotaliNutrizionali = {
  calorie: number;
  proteine_g: number;
  carboidrati_g: number;
  grassi_g: number;
  fibre_g: number;
};

export type RiferimentoLARN = TotaliNutrizionali;

export type Fascia = "bassa" | "media" | "alta";

export const DISCLAIMER_LARN =
  "Stima indicativa basata su valori di riferimento generali (LARN). Non sostituisce il parere di un professionista.";

// Fattore di attività fisica (PAL) applicato al metabolismo basale.
const PAL_PER_LIVELLO: Record<DatiBiometrici["livello_attivita"], number> = {
  sedentario: 1.3,
  moderato: 1.55,
  attivo: 1.75,
};

function metabolismoBasale(d: DatiBiometrici): number {
  // Formula di Mifflin-St Jeor.
  const base = 10 * d.peso_kg + 6.25 * d.altezza_cm - 5 * d.eta;
  return d.sesso === "M" ? base + 5 : base - 161;
}

/**
 * Riferimento LARN settimanale per il profilo, calcolato con formule
 * standard (Mifflin-St Jeor per il fabbisogno energetico, indicazioni LARN
 * per la ripartizione dei macronutrienti e per le fibre) — non richiesto
 * all'AI. Richiede tutti i dati biometrici: se manca anche solo un campo,
 * il confronto non può essere mostrato.
 */
export function calcolaRiferimentoLARN(d: DatiBiometrici): RiferimentoLARN {
  const kcalGiorno = metabolismoBasale(d) * PAL_PER_LIVELLO[d.livello_attivita];

  const proteineGrammiPerKg = d.eta >= 65 ? 1.0 : 0.9;
  const proteineGiorno = proteineGrammiPerKg * d.peso_kg;

  // Ripartizione dei macronutrienti sul punto medio dell'intervallo LARN:
  // carboidrati 45-60% delle kcal, grassi 20-35% delle kcal.
  const carboidratiGiorno = (kcalGiorno * 0.5) / 4;
  const grassiGiorno = (kcalGiorno * 0.3) / 9;

  const fibreGiorno = d.sesso === "M" ? 30 : 25;

  return {
    calorie: Math.round(kcalGiorno * 7),
    proteine_g: Math.round(proteineGiorno * 7),
    carboidrati_g: Math.round(carboidratiGiorno * 7),
    grassi_g: Math.round(grassiGiorno * 7),
    fibre_g: Math.round(fibreGiorno * 7),
  };
}

function fasciaDaRapporto(rapporto: number): Fascia {
  if (rapporto < 0.85) return "bassa";
  if (rapporto > 1.15) return "alta";
  return "media";
}

export type ConfrontoNutriente = {
  chiave: keyof TotaliNutrizionali;
  etichetta: string;
  nomeModifica: string;
  totale: number;
  riferimento: number;
  fascia: Fascia;
  unita: string;
};

const NUTRIENTI: { chiave: keyof TotaliNutrizionali; etichetta: string; nomeModifica: string; unita: string }[] = [
  { chiave: "calorie", etichetta: "Energia", nomeModifica: "l'apporto energetico (calorie)", unita: "kcal" },
  { chiave: "proteine_g", etichetta: "Proteine", nomeModifica: "l'apporto proteico", unita: "g" },
  { chiave: "carboidrati_g", etichetta: "Carboidrati", nomeModifica: "l'apporto di carboidrati", unita: "g" },
  { chiave: "grassi_g", etichetta: "Grassi", nomeModifica: "l'apporto di grassi", unita: "g" },
  { chiave: "fibre_g", etichetta: "Fibre", nomeModifica: "l'apporto di fibre", unita: "g" },
];

export function confrontaConLARN(
  totali: TotaliNutrizionali,
  riferimento: RiferimentoLARN,
): ConfrontoNutriente[] {
  return NUTRIENTI.map(({ chiave, etichetta, nomeModifica, unita }) => {
    const totale = totali[chiave];
    const rif = riferimento[chiave];
    const rapporto = rif > 0 ? totale / rif : 1;
    return {
      chiave,
      etichetta,
      nomeModifica,
      totale,
      riferimento: rif,
      fascia: fasciaDaRapporto(rapporto),
      unita,
    };
  });
}
