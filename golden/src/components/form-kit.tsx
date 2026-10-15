// Stili condivisi per i form dell'app (Onboarding e Profilo): stessi
// pannelli, stesse pillole, stessa checkbox — un'unica fonte di verità
// invece di ripetere le stesse classi in due file.

export const inputClass =
  "w-full rounded-[10px] bg-paper px-4 py-3 text-sm text-ink placeholder:text-ink/40 focus:outline-none focus:ring-2 focus:ring-accent/40";
export const labelClass = "mb-2 block text-sm font-medium text-ink/75";
export const checkboxClass = "h-5 w-5 accent-dark";
export const optionRowClass = "flex min-h-11 cursor-pointer items-center gap-3 rounded-[10px] bg-paper px-4 text-sm text-ink";

export function Pill({
  label,
  selected,
  onClick,
  compact = false,
}: {
  label: string;
  selected: boolean;
  onClick: () => void;
  /** Padding/testo ridotti, per liste a griglia fissa (es. supermercati nel Check-in). */
  compact?: boolean;
}) {
  return (
    <button
      type="button"
      onClick={onClick}
      className={`min-h-11 rounded-full font-semibold ${compact ? "w-full px-2 text-xs" : "px-4 text-sm"} ${
        selected ? "bg-dark text-dark-fill-text" : "border border-ink/20 text-ink"
      }`}
    >
      {label}
    </button>
  );
}
