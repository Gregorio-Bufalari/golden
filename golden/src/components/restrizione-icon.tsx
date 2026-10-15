import type { ReactNode } from "react";

// Icone lineari per le restrizioni alimentari (onboarding + Profilo):
// stroke 1.8px, colore dark — mai emoji, mai un colore diverso da riga a
// riga (coerenza visiva della lista).
const PATHS: Record<string, ReactNode> = {
  "Glutine (celiachia)": (
    <>
      <path d="M12 3v18" />
      <path d="M12 6c-2 0-3.5 1.2-3.5 2.6S10 11 12 11s3.5 1.2 3.5 2.6S14 16 12 16" />
      <path d="M8.5 7.5l-2-1M15.5 7.5l2-1M8.5 14.5l-2 1M15.5 14.5l2 1" />
    </>
  ),
  Lattosio: (
    <>
      <path d="M10 3h4l1 3-1.3 1.7a2 2 0 0 0-.4 1.2V19a2 2 0 0 1-2 2h0a2 2 0 0 1-2-2V8.9a2 2 0 0 0-.4-1.2L9 6l1-3Z" />
      <path d="M9.3 13h5.4" />
    </>
  ),
  Vegetariano: (
    <>
      <path d="M6 13c0-5 4-9 12-9 0 8-4 12-9 12-2 0-3-1-3-3Z" />
      <path d="M9 18c0-4 2-7 9-9" />
    </>
  ),
  Vegano: (
    <>
      <path d="M12 21v-9" />
      <path d="M12 12c0-5-3-8-7-8 0 5 3 8 7 8Z" />
      <path d="M12 12c0-4 2.5-6.5 6-6.5 0 4-2.5 6.5-6 6.5Z" />
    </>
  ),
  "Frutta a guscio": (
    <>
      <path d="M12 3c3 1 5 4 5 8s-2 9-5 10c-3-1-5-6-5-10s2-7 5-8Z" />
      <path d="M12 3v2.2M9.5 9c1-1 4-1 5 0" />
    </>
  ),
  Uova: (
    <>
      <path d="M12 21c4 0 6-3.5 6-7.5C18 8.5 15 3 12 3S6 8.5 6 13.5C6 17.5 8 21 12 21Z" />
    </>
  ),
  "Pesce/Crostacei": (
    <>
      <path d="M3 12c3-4 9-6 13-4 2.5 1.2 4 3 5-1-1 3.5-2.5 5-5 6.2-4 2-10 0-13-4Z" />
      <path d="M16 10.5l.01.01" />
      <path d="M3 12s1.5 2 3.5 2" />
    </>
  ),
  "Nessuna restrizione": (
    <>
      <circle cx="12" cy="12" r="9" />
      <path d="M8.5 12.3l2.3 2.3 4.7-5" />
    </>
  ),
};

export function RestrizioneIcon({ opzione }: { opzione: string }) {
  const content = PATHS[opzione];
  if (!content) return null;
  return (
    <svg
      width="20"
      height="20"
      viewBox="0 0 24 24"
      fill="none"
      stroke="currentColor"
      strokeWidth="1.8"
      strokeLinecap="round"
      strokeLinejoin="round"
      className="shrink-0 text-dark"
      aria-hidden="true"
    >
      {content}
    </svg>
  );
}
