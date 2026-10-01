"use client";

import { useState } from "react";
import { submitCheckin } from "./checkin-actions";

const CATEGORIE_SPRECO = ["Verdura", "Proteine", "Latticini", "Pane", "Altro"];
const RETAILER_OPTIONS = ["Esselunga", "Coop", "Conad", "Carrefour", "Lidl", "Eurospin", "Altro"];

export function CheckinForm({ token }: { token: string }) {
  const [seguitoPiano, setSeguitoPiano] = useState<boolean | null>(null);
  const [spreco, setSpreco] = useState<boolean | null>(null);
  const [categoriaSpreco, setCategoriaSpreco] = useState<string | null>(null);
  const [spesaReale, setSpesaReale] = useState("");
  const [retailer, setRetailer] = useState<string | null>(null);
  const [submitting, setSubmitting] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [inviato, setInviato] = useState(false);

  const puoInviare =
    seguitoPiano !== null && spreco !== null && (!spreco || categoriaSpreco !== null);

  async function handleSubmit() {
    if (seguitoPiano === null || spreco === null) return;

    setSubmitting(true);
    setError(null);

    const result = await submitCheckin(token, {
      seguito_piano: seguitoPiano,
      spreco,
      categoria_spreco: spreco ? categoriaSpreco : null,
      spesa_reale: spesaReale ? Number(spesaReale) : null,
      retailer_usato: retailer,
    });

    if ("error" in result) {
      setError(result.error);
    } else {
      setInviato(true);
    }
    setSubmitting(false);
  }

  if (inviato) {
    return (
      <div className="mt-8 rounded-xl border border-green-200 bg-green-50 p-5 text-center text-sm text-green-700 dark:border-green-900 dark:bg-green-950 dark:text-green-400">
        Grazie! Check-in salvato.
      </div>
    );
  }

  return (
    <div className="mt-8 flex flex-col gap-5 rounded-xl border border-zinc-200 p-5 text-left dark:border-zinc-800 print:hidden">
      <h3 className="text-lg font-semibold text-zinc-950 dark:text-zinc-50">
        Check-in della settimana
      </h3>

      <div>
        <p className="mb-2 text-sm text-zinc-700 dark:text-zinc-300">Hai seguito il piano?</p>
        <div className="flex gap-2">
          {[
            { label: "Sì", value: true },
            { label: "No", value: false },
          ].map((opt) => (
            <button
              key={opt.label}
              onClick={() => setSeguitoPiano(opt.value)}
              className={`rounded-full border px-4 py-1.5 text-sm ${
                seguitoPiano === opt.value
                  ? "border-black bg-black text-white dark:border-white dark:bg-white dark:text-black"
                  : "border-zinc-300 text-zinc-700 dark:border-zinc-700 dark:text-zinc-300"
              }`}
            >
              {opt.label}
            </button>
          ))}
        </div>
      </div>

      <div>
        <p className="mb-2 text-sm text-zinc-700 dark:text-zinc-300">Hai sprecato qualcosa?</p>
        <div className="flex gap-2">
          {[
            { label: "Sì", value: true },
            { label: "No", value: false },
          ].map((opt) => (
            <button
              key={opt.label}
              onClick={() => {
                setSpreco(opt.value);
                if (!opt.value) setCategoriaSpreco(null);
              }}
              className={`rounded-full border px-4 py-1.5 text-sm ${
                spreco === opt.value
                  ? "border-black bg-black text-white dark:border-white dark:bg-white dark:text-black"
                  : "border-zinc-300 text-zinc-700 dark:border-zinc-700 dark:text-zinc-300"
              }`}
            >
              {opt.label}
            </button>
          ))}
        </div>

        {spreco && (
          <div className="mt-2 flex flex-wrap gap-2">
            {CATEGORIE_SPRECO.map((cat) => (
              <button
                key={cat}
                onClick={() => setCategoriaSpreco(cat)}
                className={`rounded-full border px-3 py-1 text-xs ${
                  categoriaSpreco === cat
                    ? "border-black bg-black text-white dark:border-white dark:bg-white dark:text-black"
                    : "border-zinc-300 text-zinc-700 dark:border-zinc-700 dark:text-zinc-300"
                }`}
              >
                {cat}
              </button>
            ))}
          </div>
        )}
      </div>

      <div>
        <p className="mb-2 text-sm text-zinc-700 dark:text-zinc-300">
          Quanto hai speso davvero, e dove? (opzionale)
        </p>
        <div className="flex items-center gap-2">
          <span className="text-zinc-500">€</span>
          <input
            type="number"
            min={0}
            value={spesaReale}
            onChange={(e) => setSpesaReale(e.target.value)}
            placeholder="0"
            className="w-24 rounded-lg border border-zinc-300 bg-transparent px-3 py-1.5 text-sm dark:border-zinc-700"
          />
        </div>
        <div className="mt-2 flex flex-wrap gap-2">
          {RETAILER_OPTIONS.map((r) => (
            <button
              key={r}
              onClick={() => setRetailer(r)}
              className={`rounded-full border px-3 py-1 text-xs ${
                retailer === r
                  ? "border-black bg-black text-white dark:border-white dark:bg-white dark:text-black"
                  : "border-zinc-300 text-zinc-700 dark:border-zinc-700 dark:text-zinc-300"
              }`}
            >
              {r}
            </button>
          ))}
        </div>
      </div>

      {error && <p className="text-sm text-red-600 dark:text-red-400">{error}</p>}

      <button
        onClick={handleSubmit}
        disabled={!puoInviare || submitting}
        className="self-start rounded-full bg-black px-5 py-2 text-sm font-medium text-white disabled:opacity-40 dark:bg-white dark:text-black"
      >
        {submitting ? "Invio..." : "Invia check-in"}
      </button>
    </div>
  );
}
