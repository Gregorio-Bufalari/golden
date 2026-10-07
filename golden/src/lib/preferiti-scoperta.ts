import "server-only";
import type { SupabaseClient } from "@supabase/supabase-js";
import type { Pasto } from "./claude";
import type { GiornoValidato, PastoValidato } from "./piano-validazione";

export type PreferitoPerRotazione = {
  id: string;
  nome: string;
  tipo: "pranzo" | "cena";
  ingredienti: Pasto["ingredienti"];
  tempo_preparazione_min: number | null;
  nutrizione: Pasto["nutrizione"] | null;
  preparazione: string[] | null;
  ultima_proposta: string | null;
};

// Non ogni settimana: in modalità Scoperta l'utente vuole soprattutto
// provare cose nuove (vedi ISTRUZIONE_SCOPERTA in claude.ts), un Preferito
// familiare entra solo ogni tanto — una settimana su tre circa — invece di
// puntare solo a varietà pura.
const PROBABILITA_PREFERITO = 1 / 3;

/**
 * Sceglie, se capita questa settimana, un Preferito da includere nel piano
 * Scoperta. Quando capita, sceglie sempre quello riproposto meno di
 * recente tra tutti i Preferiti del profilo (mai riproposto prima di uno
 * già rivisto): a rotazione, nessuno si ripete finché ce n'è un altro in
 * attesa. `random` è iniettabile per i test; di default Math.random.
 */
export function sceglieFavoritoScoperta(
  preferiti: PreferitoPerRotazione[],
  random: () => number = Math.random,
): PreferitoPerRotazione | null {
  if (preferiti.length === 0) return null;
  if (random() >= PROBABILITA_PREFERITO) return null;

  return [...preferiti].sort((a, b) => {
    if (!a.ultima_proposta && !b.ultima_proposta) return 0;
    if (!a.ultima_proposta) return -1;
    if (!b.ultima_proposta) return 1;
    return a.ultima_proposta.localeCompare(b.ultima_proposta);
  })[0];
}

/**
 * Sostituisce, nel piano, il primo pasto dello stesso tipo (pranzo/cena)
 * del Preferito scelto con l'istantanea salvata — così il piatto incluso è
 * esattamente quello che l'utente aveva messo tra i preferiti, non una
 * reinterpretazione dell'AI. Va chiamata DOPO l'adattamento budget (non
 * prima): l'AI che riduce il costo rivede liberamente tutti i pasti e
 * potrebbe alterare anche questo se fosse già presente in quel passaggio.
 * Il chiamante deve poi ricontrollare la sicurezza sul pasto appena
 * inserito e ricalcolare la lista della spesa, dato che il suo costo non
 * ha partecipato all'ottimizzazione budget.
 */
export function includiFavoritoNelPiano(
  giorni: GiornoValidato[],
  favorito: PreferitoPerRotazione,
): GiornoValidato[] {
  const pastoFavorito: PastoValidato = {
    tipo: favorito.tipo,
    nome: favorito.nome,
    ingredienti: favorito.ingredienti,
    tempo_preparazione_min: favorito.tempo_preparazione_min ?? 30,
    nutrizione: favorito.nutrizione ?? { calorie: 0, proteine_g: 0, carboidrati_g: 0, grassi_g: 0, fibre_g: 0 },
    preparazione: favorito.preparazione ?? [],
  };

  let sostituito = false;
  return giorni.map((g) => {
    if (sostituito) return g;
    const indice = g.pasti.findIndex((p) => p.tipo === favorito.tipo);
    if (indice === -1) return g;
    sostituito = true;
    const pasti = [...g.pasti];
    pasti[indice] = pastoFavorito;
    return { ...g, pasti };
  });
}

export async function leggiPreferitiPerRotazione(
  supabase: SupabaseClient,
  profileId: string,
): Promise<PreferitoPerRotazione[]> {
  const { data } = await supabase
    .from("preferiti")
    .select("id, nome, tipo, ingredienti, tempo_preparazione_min, nutrizione, preparazione, ultima_proposta")
    .eq("profile_id", profileId);

  return (data || []) as PreferitoPerRotazione[];
}

export async function segnaPreferitoProposto(supabase: SupabaseClient, preferitoId: string): Promise<void> {
  await supabase.from("preferiti").update({ ultima_proposta: new Date().toISOString() }).eq("id", preferitoId);
}
