import "server-only";
import type { SupabaseClient } from "@supabase/supabase-js";

// Quante settimane recenti considerare per evitare di riproporre piatti già
// usati di recente in modalità Scoperta (il cui scopo esplicito è "provare
// cose diverse dal solito" — vedi ISTRUZIONE_SCOPERTA in claude.ts). SOLO
// Scoperta: in Routine lo scopo è l'opposto, ripetere apposta gli stessi
// piatti settimana dopo settimana — generateMealPlan applica questa lista
// solo quando modalita === "scoperta" (difesa in profondità, non si fida
// del solo chiamante).
export const SETTIMANE_STORICO_VARIETA = 3;

type PastoSalvato = { nome: string; ingredienti: unknown[] };
type GiornoSalvato = { pasti: PastoSalvato[] };

/**
 * Nomi distinti dei piatti usati nelle ultime `numSettimane` settimane
 * salvate per questo profilo (da `weekly_plans.meal_plan`), esclusi i pasti
 * "mangio fuori" (ingredienti vuoti: non sono ricette vere, riproporli non
 * ha senso). Array vuoto se il profilo non ha ancora piani salvati (prima
 * generazione) — nessun errore, nessun impatto sulla generazione.
 */
export async function nomiPastiRecenti(
  supabase: SupabaseClient,
  profileId: string,
  numSettimane: number = SETTIMANE_STORICO_VARIETA,
): Promise<string[]> {
  const { data } = await supabase
    .from("weekly_plans")
    .select("meal_plan")
    .eq("profile_id", profileId)
    .order("settimana", { ascending: false })
    .order("created_at", { ascending: false })
    .limit(numSettimane);

  const nomi = new Set<string>();
  for (const riga of data || []) {
    const giorni = (riga.meal_plan?.giorni ?? []) as GiornoSalvato[];
    for (const g of giorni) {
      for (const p of g.pasti) {
        if (!p.ingredienti || p.ingredienti.length === 0) continue;
        nomi.add(p.nome);
      }
    }
  }
  return [...nomi];
}
