import "server-only";
import type { SupabaseClient } from "@supabase/supabase-js";
import type { ConsumoDispensa, RimastoItem } from "./grocery";

type VoceDispensa = { nome: string; unita: string; quantita: number };

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

/** Ricostruisce nome/unità/quantità dalle chiavi `nome__unita` della dispensa. */
export function elencoDispensa(dispensa: Map<string, number>): VoceDispensa[] {
  const voci: VoceDispensa[] = [];
  for (const [k, quantita] of dispensa) {
    if (quantita <= 0) continue;
    const separatore = k.lastIndexOf("__");
    if (separatore === -1) continue;
    voci.push({ nome: k.slice(0, separatore), unita: k.slice(separatore + 2), quantita });
  }
  return voci.sort((a, b) => a.nome.localeCompare(b.nome));
}

function formattaQuantita(quantita: number): string {
  return String(Math.round(quantita * 100) / 100);
}

/**
 * Istruzione da aggiungere al prompt dell'AI: elenca gli ingredienti già
 * avanzati in dispensa/frigo, da usare attivamente nel nuovo piano invece di
 * restare inutilizzati — criterio ad alta priorità per ridurre gli sprechi,
 * ma mai a scapito di restrizioni, sicurezza o budget (ricordato
 * esplicitamente nel testo stesso).
 */
export function istruzioneDispensa(dispensa: Map<string, number>): string {
  const voci = elencoDispensa(dispensa);
  if (voci.length === 0) return "";

  const elenco = voci.map((v) => `${v.nome} (${formattaQuantita(v.quantita)}${v.unita})`).join(", ");

  return (
    "Hai già questi ingredienti avanzati in dispensa/frigo da settimane precedenti: " + elenco + ". " +
    "Usali ATTIVAMENTE in uno o più pasti di questo piano, invece di introdurne di nuovi che li lascerebbero " +
    "inutilizzati, quando compatibile con restrizioni, preferenze e obiettivo. È un criterio ad alta priorità " +
    "per ridurre gli sprechi: subito dopo restrizioni alimentari, sicurezza e budget, e prima della stagionalità."
  );
}

async function applicaDelta(
  supabase: SupabaseClient,
  profileId: string,
  dispensaIniziale: Map<string, number>,
  delta: Map<string, { nome: string; unita: string; delta: number }>,
  settimana: string,
): Promise<void> {
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

function accumula(
  delta: Map<string, { nome: string; unita: string; delta: number }>,
  items: VoceDispensa[],
  segno: 1 | -1,
) {
  for (const it of items) {
    const k = chiave(it.nome, it.unita);
    const prev = delta.get(k) || { nome: it.nome, unita: it.unita, delta: 0 };
    prev.delta += segno * it.quantita;
    delta.set(k, prev);
  }
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
  accumula(delta, consumiDispensa, -1);
  accumula(delta, rimasto, 1);
  await applicaDelta(supabase, profileId, dispensaIniziale, delta, settimana);
}

/**
 * Calcola come sarebbe la dispensa se si annullasse l'effetto di una
 * versione di piano già applicata — serve per ricalcolare correttamente una
 * modifica senza trattare gli avanzi non ancora reali di QUESTA settimana
 * come se fossero già scorte disponibili da settimane precedenti.
 */
export function dispensaSenzaVersione(
  dispensaAttuale: Map<string, number>,
  vecchiConsumi: ConsumoDispensa[],
  vecchioRimasto: RimastoItem[],
): Map<string, number> {
  const base = new Map(dispensaAttuale);
  function applica(items: VoceDispensa[], segno: 1 | -1) {
    for (const it of items) {
      const k = chiave(it.nome, it.unita);
      base.set(k, (base.get(k) || 0) + segno * it.quantita);
    }
  }
  applica(vecchiConsumi, 1); // i consumi tornano disponibili
  applica(vecchioRimasto, -1); // gli avanzi che aveva aggiunto vengono tolti
  return base;
}

/**
 * Sostituisce l'effetto sulla dispensa di una versione precedente dello
 * stesso weekly_plan con quello della versione nuova, in un solo passaggio
 * (annulla i vecchi consumi/avanzi, poi applica i nuovi) — serve quando un
 * piano viene modificato (es. da /api/piano/modifica) dopo che la
 * generazione originale aveva già aggiornato la dispensa: applicare di
 * nuovo i nuovi consumi senza prima annullare i vecchi conterebbe due volte
 * lo stesso piano.
 */
export async function sostituisciConsumiDispensa(
  supabase: SupabaseClient,
  profileId: string,
  dispensaIniziale: Map<string, number>,
  vecchiConsumi: ConsumoDispensa[],
  vecchioRimasto: RimastoItem[],
  nuoviConsumi: ConsumoDispensa[],
  nuovoRimasto: RimastoItem[],
  settimana: string,
): Promise<void> {
  const delta = new Map<string, { nome: string; unita: string; delta: number }>();
  // Annulla la versione precedente: i consumi tornano in dispensa, gli
  // avanzi che aveva aggiunto vengono tolti.
  accumula(delta, vecchiConsumi, 1);
  accumula(delta, vecchioRimasto, -1);
  // Applica la versione nuova.
  accumula(delta, nuoviConsumi, -1);
  accumula(delta, nuovoRimasto, 1);
  await applicaDelta(supabase, profileId, dispensaIniziale, delta, settimana);
}
