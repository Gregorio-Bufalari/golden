import { SUPERMERCATO_OPTIONS } from "@/lib/opzioni-profilo";
import { Pill, Select } from "./form-kit";

/**
 * Stesso elenco di supermercati ovunque si scelga un supermercato —
 * Onboarding/Profilo (il supermercato di riferimento, usato solo per
 * tarare le stime prezzo) e Check-in (il retailer usato davvero quella
 * settimana, che può differire dal riferimento e alimenta la calibrazione
 * nel tempo). Un'unica fonte di verità per entrambi i casi d'uso, non due
 * elenchi duplicati. `variant` sceglie solo la resa (chip a griglia, lo
 * stile originale usato in Check-in/Profilo, o menu a tendina, usato in
 * Onboarding) — stesse opzioni in entrambi i casi.
 */
export function SupermercatoSelector({
  value,
  onChange,
  variant = "chips",
}: {
  value: string | null;
  onChange: (supermercato: string) => void;
  variant?: "chips" | "select";
}) {
  if (variant === "select") {
    return (
      <Select
        value={value || ""}
        onChange={onChange}
        options={SUPERMERCATO_OPTIONS}
        placeholder="Seleziona un supermercato"
      />
    );
  }

  return (
    <div className="grid grid-cols-4 gap-1.5">
      {SUPERMERCATO_OPTIONS.map((opt) => (
        <Pill key={opt} label={opt} selected={value === opt} onClick={() => onChange(opt)} compact />
      ))}
    </div>
  );
}
