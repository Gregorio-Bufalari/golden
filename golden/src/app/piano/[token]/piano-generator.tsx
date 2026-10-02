"use client";

import { useState } from "react";
import { GroceryList } from "./grocery-list";
import { CheckinForm } from "./checkin-form";

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

function formatQuantita(quantita: number, unita: Ingrediente["unita"]): string {
  if (unita === "g" && quantita >= 1000) {
    return `${(quantita / 1000).toFixed(quantita % 1000 === 0 ? 0 : 1)} kg`;
  }
  if (unita === "ml" && quantita >= 1000) {
    return `${(quantita / 1000).toFixed(quantita % 1000 === 0 ? 0 : 1)} l`;
  }
  const arrotondata = Math.round(quantita * 10) / 10;
  return `${arrotondata} ${unita}`;
}

export function PianoGenerator({ token }: { token: string }) {
  const [giorni, setGiorni] = useState<Giorno[] | null>(null);
  const [groceryList, setGroceryList] = useState<GroceryListData | null>(null);
  const [settimana, setSettimana] = useState<string>("");
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [riusato, setRiusato] = useState(false);

  const [messaggio, setMessaggio] = useState("");
  const [modificando, setModificando] = useState(false);
  const [erroreModifica, setErroreModifica] = useState<string | null>(null);
  const [rifiutoModifica, setRifiutoModifica] = useState<string | null>(null);

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

  async function handleModifica() {
    if (!messaggio.trim()) return;
    setModificando(true);
    setErroreModifica(null);
    setRifiutoModifica(null);

    try {
      const res = await fetch("/api/piano/modifica", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ token, messaggio }),
      });
      const data = await res.json();

      if (!res.ok) {
        setErroreModifica(data.error || "Qualcosa è andato storto.");
        return;
      }

      if (data.modifica_applicata === false) {
        setRifiutoModifica(data.motivo_rifiuto);
        return;
      }

      setGiorni(data.giorni);
      setGroceryList(data.grocery_list);
      setMessaggio("");
    } catch {
      setErroreModifica("Qualcosa è andato storto. Riprova.");
    } finally {
      setModificando(false);
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
          <div className="flex items-center justify-between print:hidden">
            {riusato ? (
              <p className="text-sm text-zinc-500 dark:text-zinc-400">
                Piano ripreso dalla settimana scorsa (modalità routine) — prezzi e quantità
                da ricalcolare in futuro.
              </p>
            ) : (
              <span />
            )}
            <button
              onClick={handleGenerate}
              disabled={loading}
              className="rounded-full border border-zinc-300 px-4 py-1.5 text-xs font-medium text-zinc-700 disabled:opacity-50 dark:border-zinc-700 dark:text-zinc-300"
            >
              {loading ? "Genero..." : "Rigenera il piano"}
            </button>
          </div>
          {error && <p className="text-sm text-red-600 dark:text-red-400 print:hidden">{error}</p>}

          <div className="flex flex-col gap-6 print:hidden">
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
                        {pasto.ingredienti
                          .map((ing) => `${ing.nome} (${formatQuantita(ing.quantita, ing.unita)})`)
                          .join(", ")}
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

          {groceryList && <GroceryList data={groceryList} settimana={settimana} />}

          <div className="rounded-xl border border-zinc-200 p-5 dark:border-zinc-800 print:hidden">
            <h3 className="mb-2 text-sm font-semibold text-zinc-800 dark:text-zinc-200">
              Modifica il piano
            </h3>
            <p className="mb-3 text-xs text-zinc-500 dark:text-zinc-400">
              Es. &quot;giovedì mangio fuori&quot;, &quot;ho già comprato il pollo&quot;,
              &quot;spendi meno questa settimana&quot;.
            </p>
            <textarea
              value={messaggio}
              onChange={(e) => setMessaggio(e.target.value)}
              rows={2}
              placeholder="Scrivi qui la modifica..."
              className="w-full rounded-lg border border-zinc-300 bg-transparent px-3 py-2 text-sm placeholder:text-zinc-400 focus:border-black focus:outline-none dark:border-zinc-700 dark:focus:border-white"
            />
            {erroreModifica && (
              <p className="mt-2 text-sm text-red-600 dark:text-red-400">{erroreModifica}</p>
            )}
            {rifiutoModifica && (
              <div className="mt-2 rounded-lg border border-amber-200 bg-amber-50 px-3 py-2 text-sm text-amber-800 dark:border-amber-900 dark:bg-amber-950 dark:text-amber-300">
                {rifiutoModifica}
              </div>
            )}
            <button
              onClick={handleModifica}
              disabled={modificando || !messaggio.trim()}
              className="mt-3 rounded-full bg-black px-5 py-2 text-sm font-medium text-white disabled:opacity-40 dark:bg-white dark:text-black"
            >
              {modificando ? "Applico la modifica..." : "Applica modifica"}
            </button>
          </div>

          <CheckinForm token={token} />
        </div>
      )}
    </div>
  );
}
