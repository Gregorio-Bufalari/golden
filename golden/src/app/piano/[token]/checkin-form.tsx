"use client";

import { useState } from "react";
import { submitCheckin } from "./checkin-actions";
import { Spinner } from "@/components/spinner";
import { FeedbackPopup } from "@/components/feedback-popup";
import { SupermercatoSelector } from "@/components/supermercato-selector";

const CATEGORIE_SPRECO = ["Verdura", "Proteine", "Latticini", "Pane/pasta", "Altro"];

function SiNoButton({
  label,
  selected,
  onClick,
}: {
  label: string;
  selected: boolean;
  onClick: () => void;
}) {
  return (
    <button
      onClick={onClick}
      className={`min-h-11 flex-1 rounded-[10px] text-sm font-semibold ${
        selected ? "bg-accent text-accent-fill-text" : "border border-ink/25 text-ink"
      }`}
    >
      {label}
    </button>
  );
}

function PillOption({
  label,
  selected,
  onClick,
}: {
  label: string;
  selected: boolean;
  onClick: () => void;
}) {
  return (
    <button
      onClick={onClick}
      className={`min-h-11 rounded-full px-3.5 text-[13px] font-semibold ${
        selected ? "bg-paper text-accent" : "border border-ink/20 text-ink"
      }`}
    >
      {label}
    </button>
  );
}

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
      <div className="flex flex-col gap-3">
        <div className="bg-panel px-5 py-6 text-center text-sm font-medium text-ink">Grazie! Check-in salvato.</div>
        <FeedbackPopup
          token={token}
          contesto="checkin"
          domanda="Il check-in ti aiuta a tenere traccia della settimana?"
        />
      </div>
    );
  }

  return (
    <div className="flex flex-col gap-3 text-left print:hidden">
      <div className="bg-panel rounded-[14px] px-5 py-[18px]">
        <p className="text-base font-semibold text-ink">Hai seguito il piano?</p>
        <div className="mt-3.5 flex gap-2.5">
          <SiNoButton label="Sì" selected={seguitoPiano === true} onClick={() => setSeguitoPiano(true)} />
          <SiNoButton label="No" selected={seguitoPiano === false} onClick={() => setSeguitoPiano(false)} />
        </div>
      </div>

      <div className="bg-panel rounded-[14px] px-5 py-[18px]">
        <p className="text-base font-semibold text-ink">Hai sprecato qualcosa?</p>
        <div className="mt-3.5 flex gap-2.5">
          <SiNoButton
            label="No"
            selected={spreco === false}
            onClick={() => {
              setSpreco(false);
              setCategoriaSpreco(null);
            }}
          />
          <SiNoButton label="Sì" selected={spreco === true} onClick={() => setSpreco(true)} />
        </div>

        {spreco && (
          <>
            <p className="mb-2 mt-3.5 text-[13px] text-ink/65">Cosa, principalmente?</p>
            <div className="flex flex-wrap gap-2">
              {CATEGORIE_SPRECO.map((cat) => (
                <PillOption
                  key={cat}
                  label={cat}
                  selected={categoriaSpreco === cat}
                  onClick={() => setCategoriaSpreco(cat)}
                />
              ))}
            </div>
          </>
        )}
      </div>

      <div className="bg-panel rounded-[14px] px-5 py-[18px]">
        <p className="text-base font-semibold text-ink">Quanto hai speso davvero, e dove?</p>
        <p className="mt-0.5 text-xs text-ink/55">
          Opzionale. Ci aiuta a migliorare le stime dei prezzi nel tempo: il supermercato scelto qui
          è quello usato davvero questa settimana, anche se diverso da quello di riferimento in Profilo.
        </p>
        <div className="mt-3.5 flex items-center gap-2">
          <span className="text-ink/60">€</span>
          <input
            type="number"
            min={0}
            value={spesaReale}
            onChange={(e) => setSpesaReale(e.target.value)}
            placeholder="0"
            className="min-h-11 w-24 rounded-[10px] bg-paper px-3.5 font-mono text-sm text-ink focus:outline-none focus:ring-2 focus:ring-accent/40"
          />
        </div>
        <div className="mt-3">
          <SupermercatoSelector value={retailer} onChange={setRetailer} />
        </div>
      </div>

      {error && <p className="text-sm text-clay">{error}</p>}

      <button
        onClick={handleSubmit}
        disabled={!puoInviare || submitting}
        className="flex min-h-11 items-center justify-center gap-2 rounded-xl bg-accent py-3.5 text-[15px] font-semibold text-accent-fill-text disabled:opacity-40"
      >
        {submitting && <Spinner className="h-4 w-4" />}
        {submitting ? "Invio..." : "Conferma"}
      </button>
    </div>
  );
}
