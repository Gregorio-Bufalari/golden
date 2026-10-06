import { TEMPO_OPTIONS, LIVELLO_ATTIVITA_OPTIONS } from "@/lib/opzioni-profilo";
import type { ProfileData } from "./impostazioni-form";

const NON_IMPOSTATO = "Non impostato";

function formattaTempo(minuti: number | null): string {
  if (!minuti) return NON_IMPOSTATO;
  return TEMPO_OPTIONS.find((o) => o.value === minuti)?.label || `${minuti} minuti`;
}

function formattaLivelloAttivita(livello: ProfileData["livello_attivita"]): string {
  if (!livello) return NON_IMPOSTATO;
  return LIVELLO_ATTIVITA_OPTIONS.find((o) => o.value === livello)?.label || livello;
}

function formattaSesso(sesso: ProfileData["sesso"]): string {
  if (sesso === "M") return "Maschio";
  if (sesso === "F") return "Femmina";
  return NON_IMPOSTATO;
}

function Riga({ label, value }: { label: string; value: string }) {
  return (
    <div className="border-t border-ink/10 py-3 first:border-t-0 first:pt-0">
      <div className="text-xs text-ink/55">{label}</div>
      <div className="mt-0.5 text-[15px] text-ink">{value}</div>
    </div>
  );
}

export function RiepilogoProfilo({
  profile,
  onModifica,
}: {
  profile: ProfileData;
  onModifica: () => void;
}) {
  return (
    <div className="flex flex-col gap-6 pb-10 text-left">
      <div className="bg-panel rounded-[14px] p-5">
        <Riga label="Nome" value={profile.nome || NON_IMPOSTATO} />
        <Riga
          label="Restrizioni alimentari"
          value={profile.restrizioni.length > 0 ? profile.restrizioni.join(", ") : NON_IMPOSTATO}
        />
        <Riga
          label="Numero di persone"
          value={profile.household_size ? String(profile.household_size) : NON_IMPOSTATO}
        />
        <Riga label="Obiettivo" value={profile.obiettivo || NON_IMPOSTATO} />
        <Riga label="Cucina preferita" value={profile.preferenze?.cucina?.join(", ") || NON_IMPOSTATO} />
        <Riga label="Alimenti che ti piacciono" value={profile.preferenze?.graditi?.trim() || NON_IMPOSTATO} />
        <Riga
          label="Alimenti che non ti piacciono"
          value={profile.preferenze?.non_graditi?.trim() || NON_IMPOSTATO}
        />
        <Riga label="Tempo massimo per cucinare" value={formattaTempo(profile.tempo_max_cucina)} />
        <Riga
          label="Budget settimanale"
          value={profile.budget_settimanale ? `€${profile.budget_settimanale}` : NON_IMPOSTATO}
        />
        <Riga label="Supermercato" value={profile.supermercato || NON_IMPOSTATO} />
      </div>

      <div>
        <h3 className="mb-1 text-sm font-semibold text-ink">Dati biometrici</h3>
        <p className="mb-3 text-xs text-ink/55">
          Opzionali. Servono solo per mostrarti, nella sezione Menu, un confronto indicativo tra il piano e i
          valori di riferimento nutrizionali generali. Nessun dato viene usato per generare il piano.
        </p>
        <div className="bg-panel rounded-[14px] p-5">
          <Riga label="Sesso" value={formattaSesso(profile.sesso)} />
          <Riga label="Età" value={profile.eta ? String(profile.eta) : NON_IMPOSTATO} />
          <Riga label="Peso" value={profile.peso_kg ? `${profile.peso_kg} kg` : NON_IMPOSTATO} />
          <Riga label="Altezza" value={profile.altezza_cm ? `${profile.altezza_cm} cm` : NON_IMPOSTATO} />
          <Riga label="Livello di attività fisica" value={formattaLivelloAttivita(profile.livello_attivita)} />
        </div>
      </div>

      <button
        type="button"
        onClick={onModifica}
        className="flex min-h-11 items-center justify-center self-start rounded-full bg-accent px-6 text-sm font-semibold text-accent-fill-text"
      >
        Modifica
      </button>
    </div>
  );
}
