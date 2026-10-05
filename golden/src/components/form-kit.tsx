// Stili condivisi per i form dell'app (Onboarding e Profilo): stessi
// pannelli, stesse pillole, stessa checkbox — un'unica fonte di verità
// invece di ripetere le stesse classi in due file.

export const inputClass =
  "w-full rounded-[10px] bg-panel px-4 py-3 text-sm text-ink placeholder:text-ink/40 focus:outline-none focus:ring-2 focus:ring-accent/40";
export const labelClass = "mb-2 block text-sm font-medium text-ink/75";
export const checkboxClass = "h-5 w-5 accent-accent";
export const optionRowClass = "flex min-h-11 cursor-pointer items-center gap-3 rounded-[10px] bg-panel px-4 text-sm text-ink";

export function Pill({
  label,
  selected,
  onClick,
}: {
  label: string;
  selected: boolean;
  onClick: () => void;
}) {
  return (
    <button
      type="button"
      onClick={onClick}
      className={`min-h-11 rounded-full px-4 text-sm font-semibold ${
        selected ? "bg-accent text-accent-fill-text" : "border border-ink/20 text-ink"
      }`}
    >
      {label}
    </button>
  );
}
