"use client";

import { useEffect, useState } from "react";
import { inviaFeedback } from "@/app/piano/[token]/feedback-actions";
import { Spinner } from "@/components/spinner";

// Non deve comparire a ogni occasione (es. ogni check-in): tenuto in
// localStorage, non sul profilo, perché è solo per decidere "quanto
// spesso" lo rivediamo — non è un dato da salvare per l'utente.
const COOLDOWN_GIORNI = 14;

function chiaveLocalStorage(contesto: string): string {
  return `groci_feedback_${contesto}`;
}

function dovrebbeMostrarsi(contesto: string): boolean {
  try {
    const ultimo = window.localStorage.getItem(chiaveLocalStorage(contesto));
    if (!ultimo) return true;
    const giorniTrascorsi = (Date.now() - Number(ultimo)) / (1000 * 60 * 60 * 24);
    return giorniTrascorsi > COOLDOWN_GIORNI;
  } catch {
    return false;
  }
}

function segnaMostrato(contesto: string) {
  try {
    window.localStorage.setItem(chiaveLocalStorage(contesto), String(Date.now()));
  } catch {
    // localStorage non disponibile: nel peggiore dei casi ricompare più spesso, non è grave.
  }
}

/**
 * Pop-up leggero per raccogliere un feedback binario (pollice su/giù) con
 * commento facoltativo, mostrato di rado (non a ogni occasione) tramite un
 * cooldown salvato in localStorage. Il dato va in una tabella separata, a
 * uso interno — mai mostrato come punteggio all'utente.
 */
export function FeedbackPopup({
  token,
  contesto,
  domanda = "Hai trovato utile questa funzione?",
}: {
  token: string;
  contesto: string;
  domanda?: string;
}) {
  // Parte da false (identico lato server, dove window non esiste) e passa
  // a true in un effect dopo il mount: è il pattern corretto per leggere
  // un'API solo-browser come localStorage senza un mismatch di idratazione
  // (il primo render lato client deve combaciare con l'HTML del server).
  const [visibile, setVisibile] = useState(false);
  useEffect(() => {
    const mostra = dovrebbeMostrarsi(contesto);
    if (mostra) {
      segnaMostrato(contesto);
      // eslint-disable-next-line react-hooks/set-state-in-effect -- lettura one-shot da localStorage dopo il mount, non un mirror di props/state
      setVisibile(true);
    }
  }, [contesto]);
  const [risposta, setRisposta] = useState<boolean | null>(null);
  const [commento, setCommento] = useState("");
  const [submitting, setSubmitting] = useState(false);
  const [fatto, setFatto] = useState(false);

  function handleRisposta(valore: boolean) {
    setRisposta(valore);
  }

  async function handleInvia() {
    if (risposta === null || submitting) return;
    setSubmitting(true);
    await inviaFeedback(token, { contesto, risposta, commento: commento.trim() || null });
    setSubmitting(false);
    setFatto(true);
    setTimeout(() => setVisibile(false), 1800);
  }

  if (!visibile) return null;

  if (fatto) {
    return (
      <div className="bg-panel rounded-[14px] px-5 py-4 text-center text-sm font-medium text-accent">
        Grazie per il feedback!
      </div>
    );
  }

  if (risposta === null) {
    return (
      <div className="flex items-start justify-between gap-3 bg-panel rounded-[14px] px-5 py-4 text-left">
        <p className="text-sm font-medium text-ink">{domanda}</p>
        <div className="flex shrink-0 items-center gap-1.5">
          <button
            type="button"
            onClick={() => handleRisposta(true)}
            aria-label="Sì, utile"
            className="flex h-11 w-11 items-center justify-center rounded-full bg-paper text-lg"
          >
            👍
          </button>
          <button
            type="button"
            onClick={() => handleRisposta(false)}
            aria-label="No, poco utile"
            className="flex h-11 w-11 items-center justify-center rounded-full bg-paper text-lg"
          >
            👎
          </button>
          <button
            type="button"
            onClick={() => setVisibile(false)}
            aria-label="Chiudi"
            className="flex h-11 w-11 items-center justify-center text-ink/40"
          >
            ×
          </button>
        </div>
      </div>
    );
  }

  return (
    <div className="bg-panel rounded-[14px] px-5 py-4 text-left">
      <p className="text-sm font-medium text-ink">Grazie! Vuoi aggiungere un dettaglio? (facoltativo)</p>
      <textarea
        value={commento}
        onChange={(e) => setCommento(e.target.value)}
        rows={2}
        placeholder="Scrivi qui..."
        className="mt-2.5 w-full rounded-xl bg-paper px-3.5 py-3 text-sm text-ink placeholder:text-ink/40 focus:outline-none focus:ring-2 focus:ring-accent/40"
      />
      <button
        type="button"
        onClick={handleInvia}
        disabled={submitting}
        className="mt-3 flex min-h-11 items-center justify-center gap-2 rounded-full bg-accent px-5 text-sm font-semibold text-accent-fill-text disabled:opacity-50"
      >
        {submitting && <Spinner className="h-4 w-4" />}
        {submitting ? "Invio..." : "Invia"}
      </button>
    </div>
  );
}
