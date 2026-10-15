import type { ReactNode } from "react";

// Icone lineari per reparto (dettaglio piatto nel Menu): stroke 1.8px,
// colore dark. La lista della spesa resta con le emoji colorate per
// prodotto — stile deliberatamente diverso, non va toccata qui.
const PATHS: Record<string, ReactNode> = {
  "Frutta e verdura": (
    <>
      <path d="M12 21c4.5 0 7-3.5 7-8 0-3-1.8-5-4-5-1.3 0-2.2.6-3 1.6C11.2 8.6 10.3 8 9 8c-2.2 0-4 2-4 5 0 4.5 2.5 8 7 8Z" />
      <path d="M12 8c0-2 .8-3.5 2.5-4.5" />
    </>
  ),
  "Carne e pesce": (
    <>
      <path d="M4 12c3-4 9-6 13-4 2.5 1.2 4 3 5-1-1 3.5-2.5 5-5 6.2-4 2-10 0-13-4Z" />
      <path d="M16.5 10.3l.01.01" />
    </>
  ),
  "Latticini e uova": (
    <>
      <path d="M12 21c4 0 6-3.5 6-7.5C18 9.5 15 4 12 4S6 9.5 6 13.5C6 17.5 8 21 12 21Z" />
    </>
  ),
  "Pane e cereali": (
    <>
      <path d="M12 3c4.4 0 7 2.7 7 7 0 6-3 11-7 11S5 16 5 10c0-4.3 2.6-7 7-7Z" />
      <path d="M12 4v16" />
    </>
  ),
  Dispensa: (
    <>
      <rect x="5" y="4" width="14" height="17" rx="1.5" />
      <path d="M5 9h14" />
      <path d="M9 4v3M15 4v3" />
    </>
  ),
  Surgelati: (
    <>
      <path d="M12 3v18M4 7.5l16 9M20 7.5l-16 9" />
    </>
  ),
  Altro: (
    <>
      <rect x="4" y="4" width="16" height="16" rx="3" />
    </>
  ),
};

export function RepartoIcon({ reparto }: { reparto: string }) {
  const content = PATHS[reparto] ?? PATHS["Altro"];
  return (
    <svg
      width="16"
      height="16"
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
