"use server";

import { createAdminClient } from "@/lib/supabase/admin";

export async function setModalita(
  token: string,
  modalita: "routine" | "scoperta",
): Promise<{ success: true } | { error: string }> {
  const supabase = createAdminClient();

  const { error } = await supabase
    .from("profiles")
    .update({ modalita })
    .eq("link_token", token);

  if (error) {
    console.error("setModalita error:", error);
    return { error: "Non sono riuscito ad aggiornare la modalità." };
  }

  return { success: true };
}
