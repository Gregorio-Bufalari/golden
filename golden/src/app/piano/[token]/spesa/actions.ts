"use server";

import { createAdminClient } from "@/lib/supabase/admin";

/**
 * Spunta/togli la spunta "acquistato" per un prodotto della lista della
 * spesa dell'ultimo piano del profilo. Nessuna AI coinvolta: solo lettura
 * del piano più recente (per sicurezza, tramite il token — non ci si fida
 * di un weekly_plan_id passato dal client) e scrittura in spesa_stato.
 */
export async function setAcquistato(
  token: string,
  prodotto: string,
  acquistato: boolean,
): Promise<{ success: true } | { error: string }> {
  if (!token || !prodotto) {
    return { error: "Dati mancanti." };
  }

  const supabase = createAdminClient();

  const { data: profile } = await supabase
    .from("profiles")
    .select("id")
    .eq("link_token", token)
    .single();

  if (!profile) {
    return { error: "Profilo non trovato." };
  }

  const { data: ultimoPiano } = await supabase
    .from("weekly_plans")
    .select("id")
    .eq("profile_id", profile.id)
    .order("settimana", { ascending: false })
    .order("created_at", { ascending: false })
    .limit(1)
    .maybeSingle();

  if (!ultimoPiano) {
    return { error: "Nessun piano trovato." };
  }

  const { error } = await supabase.from("spesa_stato").upsert(
    { weekly_plan_id: ultimoPiano.id, prodotto, acquistato },
    { onConflict: "weekly_plan_id,prodotto" },
  );

  if (error) {
    console.error("setAcquistato error:", error);
    return { error: "Non sono riuscito a salvare. Riprova." };
  }

  return { success: true };
}
