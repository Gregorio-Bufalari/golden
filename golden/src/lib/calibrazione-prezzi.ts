import "server-only";
import type { SupabaseClient } from "@supabase/supabase-js";

// Impara dai check-in passati quanto le stime prezzo si discostano dalla
// spesa reale PER IL SUPERMERCATO DI RIFERIMENTO del profilo (non per un
// retailer qualsiasi usato una tantum — vedi fattoreCalibrazione), e
// restituisce un fattore correttivo da applicare alla stima statica per
// fascia (TIER_MOLTIPLICATORE in grocery.ts). Puramente derivato dai dati
// già raccolti nei check-in: nessuna chiamata AI, nessun nuovo dato da
// chiedere all'utente.

export type CheckinPerCalibrazione = {
  retailer_usato: string | null;
  spesa_reale: number | null;
  budget_stimato: number | null;
};

// Sotto questa soglia di campioni per quel retailer, un singolo check-in
// fuori norma potrebbe distorcere la stima: meglio restare sulla fascia
// statica (fattore 1) finché non ce ne sono abbastanza.
const CAMPIONE_MINIMO = 2;

// Limita quanto il fattore imparato può spostarsi dalla stima statica, a
// protezione da un valore inserito per errore in un singolo check-in.
const FATTORE_MIN = 0.7;
const FATTORE_MAX = 1.4;

/**
 * Fattore correttivo per il retailer indicato (in pratica, il
 * supermercato di riferimento del profilo): media del rapporto
 * spesa_reale/budget_stimato sui check-in in cui l'utente ha dichiarato
 * di aver fatto la spesa proprio lì — i check-in con un retailer diverso
 * (può succedere, è normale) non contano per QUESTA calibrazione. 1 =
 * nessuna correzione.
 */
export function fattoreCalibrazione(
  checkins: CheckinPerCalibrazione[],
  retailer: string | null,
): number {
  if (!retailer) return 1;

  const rapporti = checkins
    .filter(
      (c) =>
        c.retailer_usato === retailer &&
        c.spesa_reale !== null &&
        c.budget_stimato !== null &&
        c.budget_stimato > 0,
    )
    .map((c) => (c.spesa_reale as number) / (c.budget_stimato as number));

  if (rapporti.length < CAMPIONE_MINIMO) return 1;

  const media = rapporti.reduce((sum, r) => sum + r, 0) / rapporti.length;
  return Math.min(FATTORE_MAX, Math.max(FATTORE_MIN, media));
}

/**
 * Legge lo storico check-in del profilo da Supabase e calcola il fattore
 * di calibrazione per il retailer indicato — stesso identico calcolo
 * finora duplicato in /api/piano/generate e /api/piano/modifica,
 * centralizzato qui perché serve anche a /api/piano/conferma (ricalcola
 * la lista della spesa dello scenario scelto prima di salvarla).
 */
export async function calcolaFattoreCalibrazionePerProfilo(
  supabase: SupabaseClient,
  profileId: string,
  retailer: string | null,
): Promise<number> {
  const { data: storicoPiani } = await supabase
    .from("weekly_plans")
    .select("budget_stimato, checkins(retailer_usato, spesa_reale)")
    .eq("profile_id", profileId);

  const checkinsStorico: CheckinPerCalibrazione[] = (storicoPiani || []).flatMap((p) =>
    (p.checkins || []).map((c: { retailer_usato: string | null; spesa_reale: number | null }) => ({
      retailer_usato: c.retailer_usato,
      spesa_reale: c.spesa_reale,
      budget_stimato: p.budget_stimato,
    })),
  );

  return fattoreCalibrazione(checkinsStorico, retailer);
}
