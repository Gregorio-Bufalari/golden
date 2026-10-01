"use client";

import { useState } from "react";

type Pasto = {
  tipo: "pranzo" | "cena";
  nome: string;
  ingredienti: string[];
  tempo_preparazione_min: number;
  verificare?: boolean;
  ingredienti_a_rischio?: string[];
};

type Giorno = {
  giorno: string;
  pasti: Pasto[];
};

export function PianoGenerator({ token }: { token: string }) {
  const [giorni, setGiorni] = useState<Giorno[] | null>(null);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  async function handleGenerate() {
    setLoading(true);
    setError(null);

    try {
      const res = await fetch("/api/piano/generate", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ token }),
      });
      const data = await res.json();

      if (!res.ok) {
        setError(data.error || "Qualcosa è andato storto.");
        return;
      }

      setGiorni(data.giorni);
    } catch {
      setError("Qualcosa è andato storto. Riprova.");
    } finally {
      setLoading(false);
    }
  }

  return (
    <div className="mt-10 w-full max-w-2xl">
      {!giorni && (
        <div className="flex flex-col items-center gap-3">
          <button
            onClick={handleGenerate}
            disabled={loading}
            className="rounded-full bg-black px-6 py-2.5 text-sm font-medium text-white disabled:opacity-50 dark:bg-white dark:text-black"
          >
            {loading ? "Genero il piano..." : "Genera il piano della settimana"}
          </button>
          {error && <p className="text-sm text-red-600 dark:text-red-400">{error}</p>}
        </div>
      )}

      {giorni && (
        <div className="flex flex-col gap-6 text-left">
          {giorni.map((giorno) => (
            <div
              key={giorno.giorno}
              className="rounded-xl border border-zinc-200 p-5 dark:border-zinc-800"
            >
              <h3 className="mb-3 text-lg font-semibold text-zinc-950 dark:text-zinc-50">
                {giorno.giorno}
              </h3>
              <div className="flex flex-col gap-4">
                {giorno.pasti.map((pasto, i) => (
                  <div key={i}>
                    <div className="flex items-center gap-2">
                      <span className="text-xs font-medium uppercase text-zinc-500 dark:text-zinc-500">
                        {pasto.tipo}
                      </span>
                      <span className="font-medium text-zinc-900 dark:text-zinc-100">
                        {pasto.nome}
                      </span>
                      <span className="text-xs text-zinc-400">
                        ({pasto.tempo_preparazione_min} min)
                      </span>
                    </div>
                    <p className="mt-1 text-sm text-zinc-600 dark:text-zinc-400">
                      {pasto.ingredienti.join(", ")}
                    </p>
                    {pasto.verificare && (
                      <div className="mt-2 rounded-lg border border-red-200 bg-red-50 px-3 py-2 text-xs text-red-700 dark:border-red-900 dark:bg-red-950 dark:text-red-400">
                        ⚠️ Verifica necessaria: possibili tracce di glutine in{" "}
                        {pasto.ingredienti_a_rischio?.join(", ")}. Controlla le etichette
                        prima di procedere.
                      </div>
                    )}
                  </div>
                ))}
              </div>
            </div>
          ))}
        </div>
      )}
    </div>
  );
}
