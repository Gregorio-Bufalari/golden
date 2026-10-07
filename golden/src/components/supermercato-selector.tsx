import { SUPERMERCATO_OPTIONS } from "@/lib/opzioni-profilo";
import { Pill } from "./form-kit";

/**
 * Stesso elenco di supermercati e stesso stile di pillola ovunque si
 * scelga un supermercato — in Onboarding/Profilo (il supermercato di
 * riferimento, usato solo per tarare le stime prezzo) e nel Check-in (il
 * retailer usato davvero quella settimana, che può differire dal
 * riferimento e alimenta la calibrazione nel tempo). Un'unica fonte di
 * verità per entrambi i casi d'uso, non due elenchi duplicati.
 */
export function SupermercatoSelector({
  value,
  onChange,
}: {
  value: string | null;
  onChange: (supermercato: string) => void;
}) {
  return (
    <div className="flex flex-wrap gap-2">
      {SUPERMERCATO_OPTIONS.map((opt) => (
        <Pill key={opt} label={opt} selected={value === opt} onClick={() => onChange(opt)} />
      ))}
    </div>
  );
}
