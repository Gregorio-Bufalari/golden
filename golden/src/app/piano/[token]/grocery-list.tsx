"use client";

import { useState } from "react";
import { Spinner } from "@/components/spinner";
import { setAcquistato } from "./spesa/actions";
import { gruppoAcquisto, type GruppoAcquisto } from "@/lib/conservazione";

type GroceryItem = {
  nome: string;
  quantita: number;
  quantitaNecessaria: number;
  confezione: number | null;
  unita: "g" | "kg" | "ml" | "l" | "pz" | "confezione";
  prezzo_stimato: number;
};

type GroceryReparto = {
  reparto: string;
  items: GroceryItem[];
  subtotale: number;
};

type RimastoItem = {
  nome: string;
  quantita: number;
  quantitaNecessaria: number;
  unita: GroceryItem["unita"];
};

type GroceryListData = {
  reparti: GroceryReparto[];
  rimasto: RimastoItem[];
  totale_stimato: number;
  fascia: "discount" | "media" | "premium";
};

function formatQuantita(quantita: number, unita: GroceryItem["unita"]): string {
  if (unita === "g" && quantita >= 1000) {
    return `${(quantita / 1000).toFixed(quantita % 1000 === 0 ? 0 : 1)} kg`;
  }
  if (unita === "ml" && quantita >= 1000) {
    return `${(quantita / 1000).toFixed(quantita % 1000 === 0 ? 0 : 1)} l`;
  }
  const arrotondata = Math.round(quantita * 10) / 10;
  return `${arrotondata} ${unita}`;
}

function formatRigaLista(item: GroceryItem): string {
  const avanzo = item.quantita - item.quantitaNecessaria;
  if (item.confezione && avanzo > 0) {
    return `${item.nome} — ${formatQuantita(item.quantitaNecessaria, item.unita)} necessari (confezione ${formatQuantita(item.confezione, item.unita)}, avanzano ${formatQuantita(avanzo, item.unita)})`;
  }
  return `${item.nome} — ${formatQuantita(item.quantita, item.unita)}`;
}

// Stessa classificazione di conservazione già usata nella tab Frigo
// (src/lib/conservazione.ts), riusata qui solo per raggruppare la lista
// per urgenza d'acquisto — nessuna nuova logica, nessuna AI coinvolta.
function filtraPerGruppo(reparti: GroceryReparto[], gruppo: GruppoAcquisto): GroceryReparto[] {
  return reparti
    .map((r) => ({ ...r, items: r.items.filter((i) => gruppoAcquisto(i.nome) === gruppo) }))
    .filter((r) => r.items.length > 0);
}

function sommaPrezzi(reparti: GroceryReparto[]): number {
  return reparti.flatMap((r) => r.items).reduce((somma, i) => somma + i.prezzo_stimato, 0);
}

/**
 * Messaggio da mostrare dopo una sostituzione: confronta i nomi presenti
 * nella lista prima e dopo la modifica. Pura funzione di confronto, nessuna
 * chiamata esterna — il cambiamento vero lo decide sempre l'AI a monte.
 */
export function riepilogoSostituzione(
  vecchiReparti: GroceryReparto[],
  nuoviReparti: GroceryReparto[],
  prodotto: string,
): string {
  const vecchiNomi = new Set(vecchiReparti.flatMap((r) => r.items.map((i) => i.nome)));
  const nuoviNomi = new Set(nuoviReparti.flatMap((r) => r.items.map((i) => i.nome)));
  const aggiunti = [...nuoviNomi].filter((n) => !vecchiNomi.has(n));

  if (aggiunti.length > 0) {
    return `${prodotto} sostituito con: ${aggiunti.join(", ")}.`;
  }
  if (!nuoviNomi.has(prodotto)) {
    return `${prodotto} non è più nella lista — il piano è stato aggiornato.`;
  }
  return `Il piano è stato aggiornato per ${prodotto}.`;
}

function renderSezione(
  titolo: string,
  sottotitolo: string,
  reparti: GroceryReparto[],
  statoAcquisti: Record<string, boolean>,
  onToggle: (nome: string) => void,
  itemInCorso: string | null,
  onNonTrovato: (item: GroceryItem) => void,
) {
  if (reparti.length === 0) return null;

  return (
    <div className="mb-6 last:mb-0">
      <div className="mb-2">
        <h4 className="text-base font-semibold text-zinc-900 dark:text-zinc-100">{titolo}</h4>
        <p className="text-xs text-zinc-400">
          {sottotitolo} — ~€{sommaPrezzi(reparti).toFixed(2)}
        </p>
      </div>
      <div className="flex flex-col gap-4">
        {reparti.map((reparto) => (
          <div key={reparto.reparto}>
            <h5 className="mb-1 text-sm font-semibold text-zinc-800 dark:text-zinc-200">
              {reparto.reparto}
            </h5>
            <ul className="flex flex-col gap-1">
              {reparto.items.map((item) => {
                const acquistato = Boolean(statoAcquisti[item.nome]);
                const chiave = `sostituisci__${item.nome}`;
                const caricando = itemInCorso === chiave;
                return (
                  <li
                    key={item.nome}
                    className="flex items-center justify-between gap-3 text-sm text-zinc-600 dark:text-zinc-400"
                  >
                    <label className="flex min-w-0 cursor-pointer items-center gap-2">
                      <input
                        type="checkbox"
                        checked={acquistato}
                        onChange={() => onToggle(item.nome)}
                        className="h-4 w-4 shrink-0 print:hidden"
                      />
                      <span className={acquistato ? "text-zinc-400 line-through dark:text-zinc-600" : ""}>
                        {formatRigaLista(item)}
                      </span>
                    </label>
                    <span className="flex shrink-0 items-center gap-2">
                      <button
                        onClick={() => onNonTrovato(item)}
                        disabled={Boolean(itemInCorso)}
                        className="rounded-full border border-zinc-300 px-2 py-0.5 text-[11px] font-medium text-zinc-500 disabled:opacity-40 print:hidden dark:border-zinc-700 dark:text-zinc-400"
                      >
                        Non l&apos;ho trovato
                      </button>
                      {caricando && <Spinner className="h-3.5 w-3.5 text-zinc-400 print:hidden" />}
                      <span className="text-zinc-400">~€{item.prezzo_stimato.toFixed(2)}</span>
                    </span>
                  </li>
                );
              })}
            </ul>
          </div>
        ))}
      </div>
    </div>
  );
}

// Incremento tipico per click, diverso per unità di misura.
function stepPer(unita: GroceryItem["unita"]): number {
  switch (unita) {
    case "g":
    case "ml":
      return 50;
    case "kg":
    case "l":
      return 0.5;
    default:
      return 1;
  }
}

function buildTestoWhatsApp(data: GroceryListData, settimana: string): string {
  const righe = [`*Lista della spesa* — settimana del ${settimana}`, ""];

  for (const reparto of data.reparti) {
    righe.push(`*${reparto.reparto}*`);
    for (const item of reparto.items) {
      righe.push(`- ${formatRigaLista(item)} (~€${item.prezzo_stimato.toFixed(2)})`);
    }
    righe.push("");
  }

  righe.push(`Totale stimato: ~€${data.totale_stimato.toFixed(2)}`);

  if (data.rimasto.length > 0) {
    righe.push("", "*Rimasto in frigo/dispensa*");
    for (const item of data.rimasto) {
      righe.push(`- ${item.nome}: ${formatQuantita(item.quantita, item.unita)}`);
    }
  }

  return righe.join("\n");
}

export function GroceryList({
  token,
  initialData,
  settimana,
  initialStatoAcquisti = {},
}: {
  token: string;
  initialData: GroceryListData;
  settimana: string;
  initialStatoAcquisti?: Record<string, boolean>;
}) {
  const [data, setData] = useState(initialData);
  const [itemInCorso, setItemInCorso] = useState<string | null>(null);
  const [errore, setErrore] = useState<string | null>(null);
  const [rifiuto, setRifiuto] = useState<string | null>(null);
  const [sostituzioneInfo, setSostituzioneInfo] = useState<string | null>(null);
  const [statoAcquisti, setStatoAcquisti] = useState<Record<string, boolean>>(initialStatoAcquisti);

  // Nessuna AI coinvolta: salva subito su Supabase, con aggiornamento
  // ottimistico (torna indietro solo se il salvataggio fallisce davvero).
  async function handleToggleAcquistato(nome: string) {
    const nuovoValore = !statoAcquisti[nome];
    setStatoAcquisti((prev) => ({ ...prev, [nome]: nuovoValore }));

    const risultato = await setAcquistato(token, nome, nuovoValore);
    if ("error" in risultato) {
      setStatoAcquisti((prev) => ({ ...prev, [nome]: !nuovoValore }));
      setErrore(risultato.error);
    }
  }

  // Il numero accanto ai pulsanti è l'avanzo: + deve farlo crescere (si usa
  // MENO dell'ingrediente nel menu, ne resta di più in dispensa), - deve
  // farlo scendere (si usa DI PIÙ nel menu, ne resta meno). La quantità già
  // acquistata non cambia in nessuno dei due casi.
  async function handleCambiaUso(item: RimastoItem, direzione: 1 | -1) {
    const chiave = `${item.nome}__${item.unita}`;
    if (itemInCorso) return;

    const step = stepPer(item.unita);
    const nuovaQuantita = Math.max(step, item.quantitaNecessaria - direzione * step);
    const quantitaArrotondata = Math.round(nuovaQuantita * 100) / 100;
    const verbo = direzione === 1 ? "Riduci" : "Aumenta";
    const messaggio = `${verbo} ${item.nome} a ${quantitaArrotondata}${item.unita} questa settimana`;

    setItemInCorso(chiave);
    setErrore(null);
    setRifiuto(null);

    try {
      const res = await fetch("/api/piano/modifica", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ token, messaggio }),
      });
      const result = await res.json();

      if (!res.ok) {
        setErrore(result.error || "Qualcosa è andato storto.");
        return;
      }

      if (result.modifica_applicata === false) {
        setRifiuto(result.motivo_rifiuto);
        return;
      }

      if (result.grocery_list) {
        setData(result.grocery_list);
      }
    } catch {
      setErrore("Qualcosa è andato storto. Riprova.");
    } finally {
      setItemInCorso(null);
    }
  }

  // Riusa lo stesso motore di modifica in linguaggio naturale delle altre
  // azioni: passa sempre dalla validazione sicurezza (restrizioni, rischio
  // glutine) prima di aggiornare la lista.
  async function handleNonTrovato(item: GroceryItem) {
    const chiave = `sostituisci__${item.nome}`;
    if (itemInCorso) return;

    setItemInCorso(chiave);
    setErrore(null);
    setRifiuto(null);
    setSostituzioneInfo(null);

    try {
      const res = await fetch("/api/piano/modifica", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({
          token,
          messaggio: `Sostituisci ${item.nome} con un'alternativa sicura e simile.`,
        }),
      });
      const result = await res.json();

      if (!res.ok) {
        setErrore(result.error || "Qualcosa è andato storto.");
        return;
      }

      if (result.modifica_applicata === false) {
        setRifiuto(result.motivo_rifiuto);
        return;
      }

      if (result.grocery_list) {
        setSostituzioneInfo(riepilogoSostituzione(data.reparti, result.grocery_list.reparti, item.nome));
        setData(result.grocery_list);
      }
    } catch {
      setErrore("Qualcosa è andato storto. Riprova.");
    } finally {
      setItemInCorso(null);
    }
  }

  function handleWhatsApp() {
    const testo = buildTestoWhatsApp(data, settimana);
    window.open(`https://wa.me/?text=${encodeURIComponent(testo)}`, "_blank");
  }

  function handlePrint() {
    window.print();
  }

  return (
    <div className="mt-8 rounded-xl border border-zinc-200 p-5 dark:border-zinc-800">
      <div className="mb-4 flex items-center justify-between">
        <h3 className="text-lg font-semibold text-zinc-950 dark:text-zinc-50">
          Lista della spesa
        </h3>
        <div className="flex gap-2 print:hidden">
          <button
            onClick={handleWhatsApp}
            className="rounded-full border border-zinc-300 px-4 py-1.5 text-xs font-medium text-zinc-700 dark:border-zinc-700 dark:text-zinc-300"
          >
            Condividi su WhatsApp
          </button>
          <button
            onClick={handlePrint}
            className="rounded-full border border-zinc-300 px-4 py-1.5 text-xs font-medium text-zinc-700 dark:border-zinc-700 dark:text-zinc-300"
          >
            Esporta PDF
          </button>
        </div>
      </div>

      {errore && <p className="mb-3 text-sm text-red-600 dark:text-red-400 print:hidden">{errore}</p>}
      {rifiuto && (
        <div className="mb-3 rounded-lg border border-amber-200 bg-amber-50 px-3 py-2 text-sm text-amber-800 print:hidden dark:border-amber-900 dark:bg-amber-950 dark:text-amber-300">
          {rifiuto}
        </div>
      )}
      {sostituzioneInfo && (
        <div className="mb-3 rounded-lg border border-zinc-200 bg-zinc-50 px-3 py-2 text-sm text-zinc-700 print:hidden dark:border-zinc-800 dark:bg-zinc-900 dark:text-zinc-300">
          {sostituzioneInfo}
        </div>
      )}

      {renderSezione(
        "Da comprare subito",
        "Freschi deperibili — frutta, verdura, carne, pesce, latticini",
        filtraPerGruppo(data.reparti, "subito"),
        statoAcquisti,
        handleToggleAcquistato,
        itemInCorso,
        handleNonTrovato,
      )}
      {renderSezione(
        "Può aspettare",
        "Dispensa secca e surgelati — si conservano più a lungo",
        filtraPerGruppo(data.reparti, "puo_aspettare"),
        statoAcquisti,
        handleToggleAcquistato,
        itemInCorso,
        handleNonTrovato,
      )}

      <div className="mt-4 flex items-center justify-between border-t border-zinc-200 pt-3 text-sm font-medium dark:border-zinc-800">
        <span>Totale stimato</span>
        <span>~€{data.totale_stimato.toFixed(2)}</span>
      </div>
      <p className="mt-1 text-xs text-zinc-400">
        Le quantità sono arrotondate alla confezione reale (es. 1kg di riso, non 160g) — prezzo
        stimato sulla fascia {data.fascia === "discount" ? "discount" : data.fascia === "premium" ? "premium" : "media"}, non il prezzo reale del tuo supermercato.
      </p>

      {data.rimasto.length > 0 && (
        <div className="mt-5 rounded-lg border border-zinc-200 bg-zinc-50 p-4 dark:border-zinc-800 dark:bg-zinc-900">
          <h4 className="mb-2 text-sm font-semibold text-zinc-800 dark:text-zinc-200">
            Rimasto in frigo/dispensa
          </h4>
          <p className="mb-2 text-xs text-zinc-500 dark:text-zinc-400">
            Comprando le confezioni intere, questa settimana avanza. Premi + se vuoi che ne avanzi
            di più (il menu ne userà di meno), o − se vuoi usarne di più e farne avanzare di meno.
            La quantità già acquistata non cambia.
          </p>
          <ul className="flex flex-col gap-1">
            {data.rimasto.map((item) => {
              const chiave = `${item.nome}__${item.unita}`;
              const caricando = itemInCorso === chiave;
              return (
                <li
                  key={item.nome}
                  className="flex items-center justify-between gap-3 text-sm text-zinc-600 dark:text-zinc-400"
                >
                  <span>
                    {item.nome} — {formatQuantita(item.quantita, item.unita)}
                  </span>
                  <span className="flex items-center gap-1 print:hidden">
                    <button
                      onClick={() => handleCambiaUso(item, -1)}
                      disabled={Boolean(itemInCorso)}
                      aria-label={`Usa di più ${item.nome} (ne avanza meno)`}
                      className="flex h-6 w-6 items-center justify-center rounded-full border border-zinc-300 text-xs disabled:opacity-40 dark:border-zinc-700"
                    >
                      −
                    </button>
                    <button
                      onClick={() => handleCambiaUso(item, 1)}
                      disabled={Boolean(itemInCorso)}
                      aria-label={`Fai avanzare di più ${item.nome} (ne usa meno)`}
                      className="flex h-6 w-6 items-center justify-center rounded-full border border-zinc-300 text-xs disabled:opacity-40 dark:border-zinc-700"
                    >
                      +
                    </button>
                    {caricando && <Spinner className="h-3.5 w-3.5 text-zinc-400" />}
                  </span>
                </li>
              );
            })}
          </ul>
        </div>
      )}
    </div>
  );
}
