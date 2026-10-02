"use server";

import { createAdminClient } from "@/lib/supabase/admin";

export type CheckinInput = {
  seguito_piano: boolean;
  spreco: boolean;
  categoria_spreco: string | null;
  spesa_reale: number | null;
  retailer_usato: string | null;
};

export async function submitCheckin(
  token: string,
  input: CheckinInput,
): Promise<{ success: true } | { error: string }> {
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
    return { error: "Nessun piano a cui collegare il check-in." };
  }

  const { error } = await supabase.from("checkins").insert({
    weekly_plan_id: ultimoPiano.id,
    seguito_piano: input.seguito_piano,
    spreco: input.spreco,
    categoria_spreco: input.categoria_spreco,
    spesa_reale: input.spesa_reale,
    retailer_usato: input.retailer_usato,
  });

  if (error) {
    console.error("submitCheckin error:", error);
    return { error: "Non sono riuscito a salvare il check-in." };
  }

  return { success: true };
}
