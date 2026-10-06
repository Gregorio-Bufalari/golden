// Formattazione di una quantità con la sua unità per la UI: converte in
// kg/l quando supera 1000 g/ml, arrotonda a una cifra decimale altrimenti.
// Unica implementazione condivisa (prima duplicata in grocery-list.tsx e
// Frigo) per evitare che le due divergano.
export function formattaQuantita(quantita: number, unita: string): string {
  if (unita === "g" && quantita >= 1000) {
    return `${(quantita / 1000).toFixed(quantita % 1000 === 0 ? 0 : 1)} kg`;
  }
  if (unita === "ml" && quantita >= 1000) {
    return `${(quantita / 1000).toFixed(quantita % 1000 === 0 ? 0 : 1)} l`;
  }
  const arrotondata = Math.round(quantita * 10) / 10;
  return `${arrotondata} ${unita}`;
}
