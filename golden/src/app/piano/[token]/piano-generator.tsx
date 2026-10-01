"use client";

import { useState } from "react";
import { GroceryList } from "./grocery-list";

type Ingrediente = {
  nome: string;
  quantita: number;
  unita: "g" | "kg" | "ml" | "l" | "pz" | "confezione";
  reparto: string;
};

type Pasto = {
  tipo: "pranzo" | "cena";
  nome: string;
  ingredienti: Ingrediente[];
  tempo_preparazione_min: number;
  verificare?: boolean;
  ingredienti_a_rischio?: string[];
};

type Giorno = {
  giorno: string;
  pasti: Pasto[];
};

type GroceryReparto = {
  reparto: string;
  items: { nome: string; quantita: number; unita: Ingrediente["unita"]; prezzo_stimato: number }[];
  subtotale: number;
};

type GroceryListData = {
  reparti: GroceryReparto[];
  totale_stimato: number;
  fascia: "discount" | "media" | "premium";
};

function isDomenicaSera(): boolean {
  const now = new Date();
  return now.getDay() === 0 && now.getHours() >= 18;
}

export function PianoGenerator({ token }: { token: string }) {
  const [giorni, setGiorni] = useState<Giorno[] | null>(null);
  const [groceryList, setGroceryList] = useState<GroceryListData | null>(null);
  const [settimana, setSettimana] = useState<string>("");
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [riusato, setRiusato] = useState(false);

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
      setGroceryList(data.grocery_list);
      setSettimana(data.settimana);
      setRiusato(Boolean(data.riusato));
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
          {isDomenicaSera() && (
            <div className="mb-2 rounded-xl border border-blue-200 bg-blue-50 px-4 py-3 text-sm text-blue-800 dark:border-blue-900 dark:bg-blue-950 dark:text-blue-300">
              È domenica sera — pronto per confermare il piano della prossima settimana?
            </div>
          )}
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
          {riusato && (
            <p className="text-center text-sm text-zinc-500 dark:text-zinc-400 print:hidden">
              Piano ripreso dalla settimana scorsa (modalità routine) — prezzi e quantità
              da ricalcolare in futuro.
            </p>
          )}
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
                      {pasto.ingredienti.map((ing) => ing.nome).join(", ")}
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

          {groceryList && <GroceryList data={groceryList} settimana={settimana} />}
        </div>
      )}
    </div>
  );
}
