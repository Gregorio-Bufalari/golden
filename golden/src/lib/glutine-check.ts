// Lista statica curata a mano per il controllo di sicurezza glutine (MVP).
// In una fase successiva verrà sostituita da una fonte validata (Prontuario AIC / CREA).
const RISK_KEYWORDS = [
  "frumento",
  "grano tenero",
  "grano duro",
  "orzo",
  "segale",
  "farina 00",
  "farina di frumento",
  "farina di grano",
  "pane",
  "pasta",
  "salsa di soia",
  "besciamella",
  "dado da brodo",
  "dado vegetale",
  "malto",
  "seitan",
  "cuscus",
  "couscous",
  "farro",
  "kamut",
  "avena",
];

const SAFE_QUALIFIERS = ["senza glutine", "gluten free", "certificat"];

export function isIngredienteARischio(ingrediente: string): boolean {
  const lower = ingrediente.toLowerCase();
  if (SAFE_QUALIFIERS.some((q) => lower.includes(q))) return false;
  return RISK_KEYWORDS.some((k) => lower.includes(k));
}

export function ingredientiARischio(ingredienti: string[]): string[] {
  return ingredienti.filter(isIngredienteARischio);
}
