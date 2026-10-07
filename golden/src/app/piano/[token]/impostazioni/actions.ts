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
  sesso: "M" | "F" | null;
  eta: number | null;
  peso_kg: number | null;
  altezza_cm: number | null;
  livello_attivita: "sedentario" | "moderato" | "attivo" | null;
  obiettivi_nutrizionali: {
    calorie_min?: number | null;
    calorie_max?: number | null;
    proteine_min_g?: number | null;
    carboidrati_max_g?: number | null;
    grassi_max_g?: number | null;
  };
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
      sesso: input.sesso,
      eta: input.eta,
      peso_kg: input.peso_kg,
      altezza_cm: input.altezza_cm,
      livello_attivita: input.livello_attivita,
      obiettivi_nutrizionali: input.obiettivi_nutrizionali,
    })
    .eq("link_token", token);

  if (error) {
    console.error("updateProfilo error:", error);
    return { error: "Non sono riuscito a salvare le modifiche." };
  }

  return { success: true };
}
