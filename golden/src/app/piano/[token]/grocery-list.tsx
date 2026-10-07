"use client";

import { useState } from "react";
import { Spinner } from "@/components/spinner";
import { setAcquistato } from "./spesa/actions";
import { gruppoAcquisto, type GruppoAcquisto } from "@/lib/conservazione";
import { formattaQuantita } from "@/lib/quantita";

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
  calibrato: boolean;
};

function itemARischio(nome: string, ingredientiARischio: string[]): boolean {
  const lower = nome.toLowerCase();
  return ingredientiARischio.some((r) => lower.includes(r));
}


// Metadati della riga (quantità, confezione, avanzo): punto medio tra i
// pezzi d'informazione, non trattino lungo — si legge come un dato in una
// lista, non come un titolo di giornale.
function metadataRiga(item: GroceryItem): string {
  const avanzo = item.quantita - item.quantitaNecessaria;
  if (item.confezione && avanzo > 0) {
    return `${formattaQuantita(item.quantitaNecessaria, item.unita)} necessari · confezione ${formattaQuantita(item.confezione, item.unita)}, avanzano ${formattaQuantita(avanzo, item.unita)}`;
  }
  return formattaQuantita(item.quantita, item.unita);
}

// Stessa classificazione di conservazione già usata nella tab Frigo
// (src/lib/conservazione.ts), riusata qui solo per raggruppare la lista
// per urgenza d'acquisto — nessuna nuova logica, nessuna AI coinvolta.
function filtraPerGruppo(reparti: GroceryReparto[], gruppo: GruppoAcquisto): GroceryItem[] {
  return reparti.flatMap((r) => r.items.filter((i) => gruppoAcquisto(i.nome) === gruppo));
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
    return `${prodotto} non è più nella lista. Il piano è stato aggiornato.`;
  }
  return `Il piano è stato aggiornato per ${prodotto}.`;
}

function CheckboxGlyph({ checked }: { checked: boolean }) {
  if (checked) {
    return (
      <span className="flex h-5 w-5 items-center justify-center rounded-[5px] bg-accent">
        <svg width="13" height="13" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2.4" strokeLinecap="round" strokeLinejoin="round" className="text-accent-fill-text">
          <path d="M5 12.5l4.5 4.5L19 7" />
        </svg>
      </span>
    );
  }
  return <span className="block h-5 w-5 rounded-[5px] border-[1.8px] border-ink/70" />;
}

function renderSezione(
  titolo: string,
  items: GroceryItem[],
  statoAcquisti: Record<string, boolean>,
  onToggle: (nome: string) => void,
  itemInCorso: string | null,
  onNonTrovato: (item: GroceryItem) => void,
  ingredientiARischio: string[],
) {
  if (items.length === 0) return null;

  return (
    <div>
      <h2 className="mb-0.5 text-[13px] font-semibold text-ink/60">{titolo}</h2>
      <ul>
        {items.map((item) => {
          const acquistato = Boolean(statoAcquisti[item.nome]);
          const chiave = `sostituisci__${item.nome}`;
          const caricando = itemInCorso === chiave;
          const aRischio = itemARischio(item.nome, ingredientiARischio);
          return (
            <li key={item.nome} className="flex items-start gap-1 border-t border-ink/10 first:border-t-0">
              <button
                type="button"
                onClick={() => onToggle(item.nome)}
                aria-label={`Segna come preso: ${item.nome}`}
                className="flex h-11 w-11 shrink-0 items-center justify-center print:hidden"
              >
                <CheckboxGlyph checked={acquistato} />
              </button>
              <div className="min-w-0 flex-1 py-3">
                <div className={`text-[15px] ${acquistato ? "text-ink/40 line-through" : "text-ink"}`}>
                  {item.nome}
                </div>
                <div className={`mt-0.5 font-mono text-[12.5px] ${acquistato ? "text-ink/30" : "text-ink/55"}`}>
                  {metadataRiga(item)}
                </div>
                {aRischio && !acquistato && (
                  <div className="mt-1.5 flex items-center gap-1.5">
                    <span className="h-[7px] w-[7px] shrink-0 rounded-full bg-honey" />
                    <span className="text-xs text-honey">Controlla l&apos;etichetta prima di acquistare</span>
                  </div>
                )}
                <span className="mt-0.5 flex items-center gap-2 print:hidden">
                  <button
                    onClick={() => onNonTrovato(item)}
                    disabled={Boolean(itemInCorso)}
                    className="py-1 text-xs font-semibold text-accent disabled:opacity-40"
                  >
                    Non l&apos;ho trovato
                  </button>
                  {caricando && <Spinner className="h-3.5 w-3.5 text-ink/50" />}
                </span>
              </div>
              <div
                className={`shrink-0 py-3 font-mono text-sm ${acquistato ? "text-ink/30" : "text-ink/70"}`}
              >
                €{item.prezzo_stimato.toFixed(2)}
              </div>
            </li>
          );
        })}
      </ul>
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
  const righe = [`*Lista della spesa* · settimana del ${settimana}`, ""];

  for (const reparto of data.reparti) {
    righe.push(`*${reparto.reparto}*`);
    for (const item of reparto.items) {
      righe.push(`- ${item.nome} · ${metadataRiga(item)} (~€${item.prezzo_stimato.toFixed(2)})`);
    }
    righe.push("");
  }

  righe.push(`Totale stimato: ~€${data.totale_stimato.toFixed(2)}`);

  if (data.rimasto.length > 0) {
    righe.push("", "*Rimasto in frigo/dispensa*");
    for (const item of data.rimasto) {
      righe.push(`- ${item.nome}: ${formattaQuantita(item.quantita, item.unita)}`);
    }
  }

  return righe.join("\n");
}

export function GroceryList({
  token,
  initialData,
  settimana,
  initialStatoAcquisti = {},
  budgetSettimanale,
  ingredientiARischio = [],
}: {
  token: string;
  initialData: GroceryListData;
  settimana: string;
  initialStatoAcquisti?: Record<string, boolean>;
  budgetSettimanale?: number | null;
  ingredientiARischio?: string[];
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

  const superaBudget = Boolean(budgetSettimanale && data.totale_stimato > budgetSettimanale);
  const fasciaLabel = data.fascia === "discount" ? "discount" : data.fascia === "premium" ? "premium" : "media";

  return (
    <div className="flex flex-col gap-5 py-4">
      <div className="bg-panel rounded-[14px] px-5 py-4">
        <div className="flex items-baseline justify-between">
          <div className="text-sm font-medium text-ink/75">Totale stimato</div>
          <div className="font-mono text-xl font-semibold text-ink">€{data.totale_stimato.toFixed(2)}</div>
        </div>
        <div className={`mt-1 text-[13px] ${superaBudget ? "text-honey" : "text-ink/55"}`}>
          {budgetSettimanale
            ? superaBudget
              ? `Supera il budget di €${budgetSettimanale} fissato in Profilo`
              : `Entro il budget di €${budgetSettimanale} fissato in Profilo`
            : `Stima sulla fascia ${fasciaLabel}, non il prezzo reale del tuo supermercato`}
        </div>
        {data.calibrato && (
          <div className="mt-1 text-[13px] text-ink/55">
            Corretta in base alla tua spesa reale nei check-in passati
          </div>
        )}
      </div>

      <div className="flex gap-2 print:hidden">
        <button onClick={handleWhatsApp} className="rounded-full bg-panel px-4 py-2 text-xs font-semibold text-ink">
          Condividi su WhatsApp
        </button>
        <button onClick={handlePrint} className="rounded-full bg-panel px-4 py-2 text-xs font-semibold text-ink">
          Esporta PDF
        </button>
      </div>

      {errore && <p className="text-sm text-clay print:hidden">{errore}</p>}
      {rifiuto && <div className="bg-honey-soft px-3.5 py-2.5 text-sm text-ink print:hidden">{rifiuto}</div>}
      {sostituzioneInfo && <div className="bg-panel px-3.5 py-2.5 text-sm text-ink print:hidden">{sostituzioneInfo}</div>}

      <div className="flex flex-col gap-5">
        {renderSezione(
          "Da comprare subito",
          filtraPerGruppo(data.reparti, "subito"),
          statoAcquisti,
          handleToggleAcquistato,
          itemInCorso,
          handleNonTrovato,
          ingredientiARischio,
        )}
        {renderSezione(
          "Può aspettare",
          filtraPerGruppo(data.reparti, "puo_aspettare"),
          statoAcquisti,
          handleToggleAcquistato,
          itemInCorso,
          handleNonTrovato,
          ingredientiARischio,
        )}
      </div>

      <p className="text-xs text-ink/45">
        Le quantità sono arrotondate alla confezione reale (es. 1 kg di riso, non 160 g).
      </p>

      {data.rimasto.length > 0 && (
        <div className="bg-panel rounded-[14px] p-5">
          <h3 className="mb-2 text-sm font-semibold text-ink">Rimasto in frigo/dispensa</h3>
          <p className="mb-2 text-xs text-ink/55">
            Comprando le confezioni intere, questa settimana avanza. Premi + se vuoi che ne avanzi di più (il
            menu ne userà di meno), o − se vuoi usarne di più e farne avanzare di meno. La quantità già
            acquistata non cambia.
          </p>
          <ul className="flex flex-col">
            {data.rimasto.map((item) => {
              const chiave = `${item.nome}__${item.unita}`;
              const caricando = itemInCorso === chiave;
              return (
                <li key={item.nome} className="flex items-center justify-between gap-3 border-t border-ink/10 py-1 first:border-t-0">
                  <span className="text-sm text-ink">
                    {item.nome} · {formattaQuantita(item.quantita, item.unita)}
                  </span>
                  <span className="flex items-center gap-0.5 print:hidden">
                    <button
                      onClick={() => handleCambiaUso(item, -1)}
                      disabled={Boolean(itemInCorso)}
                      aria-label={`Usa di più ${item.nome} (ne avanza meno)`}
                      className="flex h-11 w-11 items-center justify-center text-ink disabled:opacity-40"
                    >
                      <span className="flex h-7 w-7 items-center justify-center rounded-full border border-ink/20 text-xs">
                        −
                      </span>
                    </button>
                    <button
                      onClick={() => handleCambiaUso(item, 1)}
                      disabled={Boolean(itemInCorso)}
                      aria-label={`Fai avanzare di più ${item.nome} (ne usa meno)`}
                      className="flex h-11 w-11 items-center justify-center text-ink disabled:opacity-40"
                    >
                      <span className="flex h-7 w-7 items-center justify-center rounded-full border border-ink/20 text-xs">
                        +
                      </span>
                    </button>
                    {caricando && <Spinner className="h-3.5 w-3.5 text-ink/50" />}
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
