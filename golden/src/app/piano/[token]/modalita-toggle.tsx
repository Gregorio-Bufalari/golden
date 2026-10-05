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
    <div className={`flex w-full rounded-[10px] bg-panel p-[3px] transition-opacity ${disabled ? "opacity-50" : ""}`}>
      <button
        onClick={() => onSwitch("routine")}
        disabled={disabled}
        className={`flex-1 rounded-lg py-2.5 text-[13px] font-semibold transition-colors ${
          modalita === "routine" ? "bg-accent text-accent-fill-text" : "text-ink"
        }`}
      >
        Routine
      </button>
      <button
        onClick={() => onSwitch("scoperta")}
        disabled={disabled}
        className={`flex-1 rounded-lg py-2.5 text-[13px] font-medium transition-colors ${
          modalita === "scoperta" ? "bg-accent text-accent-fill-text" : "text-ink"
        }`}
      >
        Scoperta
      </button>
    </div>
  );
}
