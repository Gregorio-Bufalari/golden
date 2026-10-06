"use server";

import { createAdminClient } from "@/lib/supabase/admin";

export type FeedbackInput = {
  contesto: string;
  risposta: boolean;
  commento: string | null;
};

/**
 * Feedback rapido (pollice su/giù + commento facoltativo) salvato in una
 * tabella separata, a uso interno — mai mostrato come punteggio
 * all'utente. Nessuna chiamata AI, nessun collegamento a un piano
 * specifico: solo profilo, contesto e risposta.
 */
export async function inviaFeedback(
  token: string,
  input: FeedbackInput,
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

  const { error } = await supabase.from("feedback_rapido").insert({
    profile_id: profile.id,
    contesto: input.contesto,
    risposta: input.risposta,
    commento: input.commento,
  });

  if (error) {
    console.error("inviaFeedback error:", error);
    return { error: "Non sono riuscito a salvare il feedback." };
  }

  return { success: true };
}
