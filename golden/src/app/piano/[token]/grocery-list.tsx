"use client";

import { useState } from "react";

type GroceryItem = {
  nome: string;
  quantita: number;
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
      righe.push(
        `- ${item.nome}: ${formatQuantita(item.quantita, item.unita)} (~€${item.prezzo_stimato.toFixed(2)})`,
      );
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
}: {
  token: string;
  initialData: GroceryListData;
  settimana: string;
}) {
  const [data, setData] = useState(initialData);
  const [itemInCorso, setItemInCorso] = useState<string | null>(null);
  const [errore, setErrore] = useState<string | null>(null);
  const [rifiuto, setRifiuto] = useState<string | null>(null);

  async function handleCambiaQuantita(item: GroceryItem, direzione: 1 | -1) {
    const chiave = `${item.nome}__${item.unita}`;
    if (itemInCorso) return;

    const step = stepPer(item.unita);
    const nuovaQuantita = Math.max(step, item.quantita + direzione * step);
    const quantitaArrotondata = Math.round(nuovaQuantita * 100) / 100;
    const verbo = direzione === 1 ? "Aumenta" : "Riduci";
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

      <div className="flex flex-col gap-4">
        {data.reparti.map((reparto) => (
          <div key={reparto.reparto}>
            <h4 className="mb-1 text-sm font-semibold text-zinc-800 dark:text-zinc-200">
              {reparto.reparto}
            </h4>
            <ul className="flex flex-col gap-1">
              {reparto.items.map((item) => {
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
                    <span className="flex items-center gap-2">
                      <span className="text-zinc-400">~€{item.prezzo_stimato.toFixed(2)}</span>
                      <span className="flex items-center gap-1 print:hidden">
                        <button
                          onClick={() => handleCambiaQuantita(item, -1)}
                          disabled={Boolean(itemInCorso)}
                          aria-label={`Riduci ${item.nome}`}
                          className="flex h-6 w-6 items-center justify-center rounded-full border border-zinc-300 text-xs disabled:opacity-40 dark:border-zinc-700"
                        >
                          −
                        </button>
                        <button
                          onClick={() => handleCambiaQuantita(item, 1)}
                          disabled={Boolean(itemInCorso)}
                          aria-label={`Aumenta ${item.nome}`}
                          className="flex h-6 w-6 items-center justify-center rounded-full border border-zinc-300 text-xs disabled:opacity-40 dark:border-zinc-700"
                        >
                          +
                        </button>
                        {caricando && <span className="text-xs text-zinc-400">...</span>}
                      </span>
                    </span>
                  </li>
                );
              })}
            </ul>
          </div>
        ))}
      </div>

      <div className="mt-4 flex items-center justify-between border-t border-zinc-200 pt-3 text-sm font-medium dark:border-zinc-800">
        <span>Totale stimato</span>
        <span>~€{data.totale_stimato.toFixed(2)}</span>
      </div>
      <p className="mt-1 text-xs text-zinc-400">
        Le quantità sono arrotondate alla confezione reale (es. 1kg di riso, non 160g) — prezzo
        stimato sulla fascia {data.fascia === "discount" ? "discount" : data.fascia === "premium" ? "premium" : "media"}, non il prezzo reale del tuo supermercato. Usa i pulsanti +/- per
        cambiare una quantità: il piano della settimana si aggiorna di conseguenza.
      </p>

      {data.rimasto.length > 0 && (
        <div className="mt-5 rounded-lg border border-zinc-200 bg-zinc-50 p-4 dark:border-zinc-800 dark:bg-zinc-900">
          <h4 className="mb-2 text-sm font-semibold text-zinc-800 dark:text-zinc-200">
            Rimasto in frigo/dispensa
          </h4>
          <p className="mb-2 text-xs text-zinc-500 dark:text-zinc-400">
            Comprando le confezioni intere, questa settimana avanza:
          </p>
          <ul className="flex flex-col gap-1">
            {data.rimasto.map((item) => (
              <li key={item.nome} className="text-sm text-zinc-600 dark:text-zinc-400">
                {item.nome} — {formatQuantita(item.quantita, item.unita)}
              </li>
            ))}
          </ul>
        </div>
      )}
    </div>
  );
}
