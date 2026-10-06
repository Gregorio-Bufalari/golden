"use server";

import { createAdminClient } from "@/lib/supabase/admin";

export type IngredientePreferito = {
  nome: string;
  quantita: number;
  unita: string;
  reparto: string;
  prezzo_stimato_eur: number;
};

export type NutrizionePreferito = {
  calorie: number;
  proteine_g: number;
  carboidrati_g: number;
  grassi_g: number;
  fibre_g: number;
};

export type PastoPerPreferito = {
  tipo: "pranzo" | "cena";
  nome: string;
  ingredienti: IngredientePreferito[];
  tempo_preparazione_min: number;
  nutrizione: NutrizionePreferito;
  preparazione?: string[];
};

/**
 * Salva un'istantanea del piatto tra i preferiti (versione semplice:
 * nessuna influenza sul motore di generazione, solo una lista
 * consultabile). Idempotente: se il profilo ha già un preferito con lo
 * stesso nome, non fa nulla invece di duplicarlo.
 */
export async function aggiungiPreferito(
  token: string,
  pasto: PastoPerPreferito,
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

  const { error } = await supabase.from("preferiti").upsert(
    {
      profile_id: profile.id,
      nome: pasto.nome,
      tipo: pasto.tipo,
      ingredienti: pasto.ingredienti,
      tempo_preparazione_min: pasto.tempo_preparazione_min,
      nutrizione: pasto.nutrizione,
      preparazione: pasto.preparazione || null,
    },
    { onConflict: "profile_id,nome", ignoreDuplicates: true },
  );

  if (error) {
    console.error("aggiungiPreferito error:", error);
    return { error: "Non sono riuscito a salvare il preferito." };
  }

  return { success: true };
}

export async function rimuoviPreferito(
  token: string,
  nome: string,
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

  const { error } = await supabase
    .from("preferiti")
    .delete()
    .eq("profile_id", profile.id)
    .eq("nome", nome);

  if (error) {
    console.error("rimuoviPreferito error:", error);
    return { error: "Non sono riuscito a rimuovere il preferito." };
  }

  return { success: true };
}
