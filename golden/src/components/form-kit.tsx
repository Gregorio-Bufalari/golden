// Stili condivisi per i form dell'app (Onboarding e Profilo): stessi
// pannelli, stesse pillole, stessa checkbox — un'unica fonte di verità
// invece di ripetere le stesse classi in due file.

export const inputClass =
  "w-full rounded-[10px] bg-paper px-4 py-3 text-sm text-ink placeholder:text-ink/40 focus:outline-none focus:ring-2 focus:ring-accent/40";
export const labelClass = "mb-2 block text-sm font-medium text-ink/75";
export const checkboxClass = "h-5 w-5 accent-dark";
export const optionRowClass = "flex min-h-11 cursor-pointer items-center gap-3 rounded-[10px] bg-paper px-4 text-sm text-ink";

/**
 * Stesso aspetto degli input di testo (sfondo #FBFAF6/bg, nessun bordo,
 * radius 14px, padding 14px, Jost 15px) con una freccia a destra — per i
 * campi che il design ha convertito da chip/radio a menu a tendina
 * (Onboarding: persone a tavola, cucina preferita, supermercato).
 */
export function Select({
  value,
  onChange,
  options,
  placeholder,
}: {
  value: string;
  onChange: (value: string) => void;
  options: readonly string[] | readonly { value: string; label: string }[];
  placeholder?: string;
}) {
  return (
    <div className="relative">
      <select
        value={value}
        onChange={(e) => onChange(e.target.value)}
        className="w-full appearance-none rounded-[14px] bg-bg px-[14px] py-[14px] pr-10 text-[15px] text-ink focus:outline-none focus:ring-2 focus:ring-accent/40"
      >
        {placeholder && (
          <option value="" disabled>
            {placeholder}
          </option>
        )}
        {options.map((opt) =>
          typeof opt === "string" ? (
            <option key={opt} value={opt}>
              {opt}
            </option>
          ) : (
            <option key={opt.value} value={opt.value}>
              {opt.label}
            </option>
          ),
        )}
      </select>
      <svg
        width="14"
        height="14"
        viewBox="0 0 24 24"
        fill="none"
        stroke="currentColor"
        strokeWidth="2"
        strokeLinecap="round"
        strokeLinejoin="round"
        className="pointer-events-none absolute right-4 top-1/2 -translate-y-1/2 text-ink/50"
      >
        <path d="M6 9l6 6 6-6" />
      </svg>
    </div>
  );
}

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
