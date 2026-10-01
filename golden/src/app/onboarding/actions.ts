"use server";

import { createAdminClient } from "@/lib/supabase/admin";

export type OnboardingInput = {
  nome: string;
  restrizioni: string[];
  household_size: number;
  obiettivo: string;
  preferenze: {
    cucina: string[];
    graditi: string;
    non_graditi: string;
  };
  tempo_max_cucina: number;
  budget_settimanale: number;
  supermercato: string;
};

export async function createProfile(
  input: OnboardingInput,
): Promise<{ token: string } | { error: string }> {
  if (!input.nome.trim()) {
    return { error: "Il nome è obbligatorio." };
  }
  if (!input.restrizioni || input.restrizioni.length === 0) {
    return { error: "Seleziona almeno un'opzione per le restrizioni alimentari." };
  }

  const supabase = createAdminClient();

  const { data, error } = await supabase
    .from("profiles")
    .insert({
      nome: input.nome.trim(),
      restrizioni: input.restrizioni,
      household_size: input.household_size || null,
      obiettivo: input.obiettivo || null,
      preferenze: input.preferenze,
      tempo_max_cucina: input.tempo_max_cucina || null,
      budget_settimanale: input.budget_settimanale || null,
      supermercato: input.supermercato || null,
    })
    .select("link_token")
    .single();

  if (error || !data) {
    console.error("createProfile error:", error);
    return { error: "Non sono riuscito a salvare il profilo. Riprova." };
  }

  return { token: data.link_token as string };
}
