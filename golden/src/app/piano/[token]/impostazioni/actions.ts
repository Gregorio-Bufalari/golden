"use server";

import { createAdminClient } from "@/lib/supabase/admin";

export type ImpostazioniInput = {
  nome: string;
  restrizioni: string[];
  household_size: number | null;
  obiettivo: string;
  preferenze: { cucina: string[]; graditi: string; non_graditi: string };
  tempo_max_cucina: number | null;
  budget_settimanale: number | null;
  supermercato: string;
};

export async function updateProfilo(
  token: string,
  input: ImpostazioniInput,
): Promise<{ success: true } | { error: string }> {
  if (!input.nome.trim()) {
    return { error: "Il nome è obbligatorio." };
  }
  if (!input.restrizioni || input.restrizioni.length === 0) {
    return { error: "Seleziona almeno un'opzione per le restrizioni alimentari." };
  }

  const supabase = createAdminClient();

  const { error } = await supabase
    .from("profiles")
    .update({
      nome: input.nome.trim(),
      restrizioni: input.restrizioni,
      household_size: input.household_size,
      obiettivo: input.obiettivo || null,
      preferenze: input.preferenze,
      tempo_max_cucina: input.tempo_max_cucina,
      budget_settimanale: input.budget_settimanale,
      supermercato: input.supermercato || null,
    })
    .eq("link_token", token);

  if (error) {
    console.error("updateProfilo error:", error);
    return { error: "Non sono riuscito a salvare le modifiche." };
  }

  return { success: true };
}
