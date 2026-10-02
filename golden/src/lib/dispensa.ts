import "server-only";
import type { SupabaseClient } from "@supabase/supabase-js";
import type { ConsumoDispensa, RimastoItem } from "./grocery";

function chiave(ingrediente: string, unita: string): string {
  return `${ingrediente.toLowerCase()}__${unita}`;
}

/** Legge il saldo corrente della dispensa per un profilo. */
export async function leggiDispensa(
  supabase: SupabaseClient,
  profileId: string,
): Promise<Map<string, number>> {
  const { data: righe } = await supabase
    .from("rimanenze")
    .select("ingrediente, unita, quantita")
    .eq("profile_id", profileId);

  const dispensa = new Map<string, number>();
  for (const r of righe || []) {
    dispensa.set(chiave(r.ingrediente, r.unita), Number(r.quantita));
  }
  return dispensa;
}

/**
 * Applica a `rimanenze` i consumi (scala il saldo) e i nuovi avanzi
 * (aggiunge al saldo) risultanti dall'ultima lista della spesa calcolata.
 * Va chiamata una sola volta, con il risultato finale (dopo eventuali
 * correzioni per il budget), non ad ogni tentativo intermedio.
 */
export async function applicaConsumiDispensa(
  supabase: SupabaseClient,
  profileId: string,
  dispensaIniziale: Map<string, number>,
  consumiDispensa: ConsumoDispensa[],
  rimasto: RimastoItem[],
  settimana: string,
): Promise<void> {
  const delta = new Map<string, { nome: string; unita: string; delta: number }>();

  for (const c of consumiDispensa) {
    const k = chiave(c.nome, c.unita);
    const prev = delta.get(k) || { nome: c.nome, unita: c.unita, delta: 0 };
    prev.delta -= c.quantita;
    delta.set(k, prev);
  }

  for (const r of rimasto) {
    const k = chiave(r.nome, r.unita);
    const prev = delta.get(k) || { nome: r.nome, unita: r.unita, delta: 0 };
    prev.delta += r.quantita;
    delta.set(k, prev);
  }

  for (const [k, { nome, unita, delta: d }] of delta) {
    const saldoAttuale = dispensaIniziale.get(k) || 0;
    const nuovoSaldo = Math.round((saldoAttuale + d) * 100) / 100;

    if (nuovoSaldo <= 0) {
      await supabase
        .from("rimanenze")
        .delete()
        .eq("profile_id", profileId)
        .eq("ingrediente", nome)
        .eq("unita", unita);
    } else {
      await supabase.from("rimanenze").upsert(
        { profile_id: profileId, ingrediente: nome, unita, quantita: nuovoSaldo, settimana },
        { onConflict: "profile_id,ingrediente,unita" },
      );
    }
  }
}
