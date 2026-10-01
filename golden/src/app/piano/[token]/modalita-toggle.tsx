"use client";

import { useState } from "react";
import { setModalita } from "./actions";

export function ModalitaToggle({
  token,
  initialModalita,
}: {
  token: string;
  initialModalita: "routine" | "scoperta";
}) {
  const [modalita, setModalitaState] = useState(initialModalita);
  const [saving, setSaving] = useState(false);

  async function handleSwitch(nuova: "routine" | "scoperta") {
    if (nuova === modalita) return;
    setSaving(true);
    const prev = modalita;
    setModalitaState(nuova);
    const result = await setModalita(token, nuova);
    if ("error" in result) {
      setModalitaState(prev);
    }
    setSaving(false);
  }

  return (
    <div className="mt-6 flex items-center gap-2 text-sm">
      <span className="text-zinc-500 dark:text-zinc-400">Modalità:</span>
      <div className="flex rounded-full border border-zinc-200 p-0.5 dark:border-zinc-800">
        <button
          onClick={() => handleSwitch("routine")}
          disabled={saving}
          className={`rounded-full px-3 py-1 transition-colors ${
            modalita === "routine"
              ? "bg-black text-white dark:bg-white dark:text-black"
              : "text-zinc-600 dark:text-zinc-400"
          }`}
        >
          Routine
        </button>
        <button
          onClick={() => handleSwitch("scoperta")}
          disabled={saving}
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
