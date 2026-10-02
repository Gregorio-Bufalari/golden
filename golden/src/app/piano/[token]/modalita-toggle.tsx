"use client";

export function ModalitaToggle({
  modalita,
  onSwitch,
  disabled,
}: {
  modalita: "routine" | "scoperta";
  onSwitch: (nuova: "routine" | "scoperta") => void;
  disabled?: boolean;
}) {
  return (
    <div className="flex items-center gap-2 text-sm">
      <span className="text-zinc-500 dark:text-zinc-400">Modalità:</span>
      <div
        className={`flex rounded-full border border-zinc-200 p-0.5 transition-opacity dark:border-zinc-800 ${
          disabled ? "opacity-50" : ""
        }`}
      >
        <button
          onClick={() => onSwitch("routine")}
          disabled={disabled}
          className={`rounded-full px-3 py-1 transition-colors ${
            modalita === "routine"
              ? "bg-black text-white dark:bg-white dark:text-black"
              : "text-zinc-600 dark:text-zinc-400"
          }`}
        >
          Routine
        </button>
        <button
          onClick={() => onSwitch("scoperta")}
          disabled={disabled}
          className={`rounded-full px-3 py-1 transition-colors ${
            modalita === "scoperta"
              ? "bg-black text-white dark:bg-white dark:text-black"
              : "text-zinc-600 dark:text-zinc-400"
          }`}
        >
          Scoperta
        </button>
      </div>
    </div>
  );
}
