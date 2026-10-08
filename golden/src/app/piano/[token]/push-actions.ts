"use server";

import { createAdminClient } from "@/lib/supabase/admin";

export type SottoscrizionePushInput = {
  endpoint: string;
  keys: { p256dh: string; auth: string };
};

/**
 * Salva (o aggiorna, se lo stesso endpoint esiste già) la sottoscrizione
 * push di questo dispositivo/browser per il profilo. Un profilo può avere
 * più sottoscrizioni attive (telefono + desktop).
 */
export async function salvaPushSubscription(
  token: string,
  subscription: SottoscrizionePushInput,
): Promise<{ success: true } | { error: string }> {
  if (!subscription?.endpoint || !subscription.keys?.p256dh || !subscription.keys?.auth) {
    return { error: "Sottoscrizione non valida." };
  }

  const supabase = createAdminClient();

  const { data: profile } = await supabase.from("profiles").select("id").eq("link_token", token).single();
  if (!profile) {
    return { error: "Profilo non trovato." };
  }

  const { error } = await supabase.from("push_subscriptions").upsert(
    {
      profile_id: profile.id,
      endpoint: subscription.endpoint,
      p256dh: subscription.keys.p256dh,
      auth_key: subscription.keys.auth,
    },
    { onConflict: "endpoint" },
  );

  if (error) {
    console.error("salvaPushSubscription error:", error);
    return { error: "Non sono riuscito ad attivare le notifiche." };
  }

  return { success: true };
}

export async function rimuoviPushSubscription(
  token: string,
  endpoint: string,
): Promise<{ success: true } | { error: string }> {
  const supabase = createAdminClient();

  const { data: profile } = await supabase.from("profiles").select("id").eq("link_token", token).single();
  if (!profile) {
    return { error: "Profilo non trovato." };
  }

  const { error } = await supabase
    .from("push_subscriptions")
    .delete()
    .eq("profile_id", profile.id)
    .eq("endpoint", endpoint);

  if (error) {
    console.error("rimuoviPushSubscription error:", error);
    return { error: "Non sono riuscito a disattivare le notifiche." };
  }

  return { success: true };
}
