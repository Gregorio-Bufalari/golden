"use client";

import { useState } from "react";
import { rimuoviPreferito } from "../preferiti-actions";
import { HeartIcon } from "@/components/heart-icon";

type Ingrediente = { nome: string; quantita: number; unita: string; reparto: string; prezzo_stimato_eur: number };
type Nutrizione = { calorie: number; proteine_g: number; carboidrati_g: number; grassi_g: number; fibre_g: number };

export type Preferito = {
  nome: string;
  tipo: "pranzo" | "cena";
  ingredienti: Ingrediente[];
  tempo_preparazione_min: number | null;
  nutrizione: Nutrizione | null;
  preparazione: string[] | null;
};

function ChevronIcon({ aperto }: { aperto: boolean }) {
  return (
    <svg
      width="14"
      height="14"
      viewBox="0 0 24 24"
      fill="none"
      stroke="currentColor"
      strokeWidth="2"
      strokeLinecap="round"
      strokeLinejoin="round"
      className={`shrink-0 transition-transform ${aperto ? "rotate-180" : ""}`}
    >
      <path d="M6 9l6 6 6-6" />
    </svg>
  );
}

export function PreferitiView({ token, initialPreferiti }: { token: string; initialPreferiti: Preferito[] }) {
  const [preferiti, setPreferiti] = useState(initialPreferiti);
  const [espanso, setEspanso] = useState<string | null>(null);

  async function handleRimuovi(nome: string) {
    const precedenti = preferiti;
    setPreferiti((prev) => prev.filter((p) => p.nome !== nome));

    const result = await rimuoviPreferito(token, nome);
    if ("error" in result) {
      setPreferiti(precedenti);
    }
  }

  if (preferiti.length === 0) {
    return (
      <p className="pt-10 text-center text-sm text-ink/55">
        Nessun piatto salvato ancora. Tocca il cuore su un piatto nel Menu per aggiungerlo qui.
      </p>
    );
  }

  return (
    <div className="flex flex-col gap-3 py-4">
      {preferiti.map((p) => {
        const haPreparazione = Boolean(p.preparazione?.length);
        const aperto = espanso === p.nome;
        return (
          <div key={p.nome} className="flex flex-col gap-2">
            <div className="relative">
              <button
                type="button"
                onClick={() => haPreparazione && setEspanso(aperto ? null : p.nome)}
                disabled={!haPreparazione}
                className="block w-full rounded-[14px] bg-panel px-5 py-[18px] pr-14 text-left disabled:cursor-default"
              >
                <div className="text-[13px] font-medium text-ink/60">
                  {p.tipo === "pranzo" ? "Pranzo" : "Cena"}
                  {p.tempo_preparazione_min ? ` · ${p.tempo_preparazione_min} min` : ""}
                </div>
                <div className="mt-1 text-[21px] font-bold leading-tight tracking-tight text-ink">{p.nome}</div>
                <p className="mt-1.5 text-[13px] text-ink/65">
                  {p.ingredienti.map((ing) => ing.nome).join(", ")}
                </p>
                {p.nutrizione && (
                  <div className="mt-3.5 flex flex-wrap items-center gap-x-3 gap-y-1 font-mono text-[13px] text-ink/75">
                    <span>{p.nutrizione.calorie} kcal</span>
                    <span>{p.nutrizione.proteine_g}g proteine</span>
                    <span>{p.nutrizione.carboidrati_g}g carboidrati</span>
                    <span>{p.nutrizione.grassi_g}g grassi</span>
                    <span>{p.nutrizione.fibre_g}g fibre</span>
                  </div>
                )}
                {haPreparazione && (
                  <span className="mt-2 flex items-center gap-1 font-sans text-xs font-medium text-accent">
                    Preparazione
                    <ChevronIcon aperto={aperto} />
                  </span>
                )}
              </button>
              <button
                type="button"
                onClick={() => handleRimuovi(p.nome)}
                aria-label={`Rimuovi ${p.nome} dai preferiti`}
                aria-pressed="true"
                className="absolute right-3 top-3 flex h-9 w-9 items-center justify-center text-accent"
              >
                <HeartIcon pieno />
              </button>
            </div>

            {aperto && haPreparazione && (
              <ol className="flex list-decimal flex-col gap-1 rounded-[14px] bg-panel px-5 py-4 pl-9 text-sm text-ink/80">
                {p.preparazione?.map((passo, j) => (
                  <li key={j}>{passo}</li>
                ))}
              </ol>
            )}
          </div>
        );
      })}
    </div>
  );
}
