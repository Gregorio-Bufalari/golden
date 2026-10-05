// Pura funzione di derivazione (nessuna AI, nessun hook): vive in un file
// SENZA "use client" così da poter essere chiamata direttamente sia dal
// Server Component di Spesa sia dal Client Component GroceryList. Prima
// viveva in grocery-list.tsx ("use client"): chiamarla come funzione
// normale da un Server Component faceva crashare la pagina ("Attempted
// to call ... from the server, but ... is on the client").
export type PastoPerRischio = { verificare?: boolean; ingredienti_a_rischio?: string[] };
export type GiornoPerRischio = { pasti: PastoPerRischio[] };

/**
 * Ingredienti segnalati "da verificare" (rischio glutine) in uno o più
 * pasti della settimana, in minuscolo — per mostrare lo stesso avviso
 * anche sulla riga della lista della spesa, non solo sul piatto nel Menu.
 */
export function ingredientiARischioSettimana(giorni: GiornoPerRischio[]): string[] {
  const nomi = new Set<string>();
  for (const giorno of giorni) {
    for (const pasto of giorno.pasti || []) {
      if (pasto.verificare) {
        for (const n of pasto.ingredienti_a_rischio || []) nomi.add(n.toLowerCase());
      }
    }
  }
  return [...nomi];
}
