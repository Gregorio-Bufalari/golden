import Link from "next/link";
import { Logo } from "@/components/logo";

function ProfiloButton({ token }: { token: string }) {
  return (
    <Link
      href={`/piano/${token}/impostazioni`}
      aria-label="Profilo"
      className="flex h-11 w-11 shrink-0 items-center justify-center rounded-full bg-paper print:hidden"
    >
      <svg
        width="18"
        height="18"
        viewBox="0 0 24 24"
        fill="none"
        stroke="currentColor"
        strokeWidth="1.6"
        strokeLinecap="round"
        strokeLinejoin="round"
        className="text-ink"
      >
        <circle cx="12" cy="8" r="4" />
        <path d="M4 21c0-4.4 3.6-7 8-7s8 2.6 8 7" />
      </svg>
    </Link>
  );
}

function BackButton({ href }: { href: string }) {
  return (
    <Link href={href} aria-label="Indietro" className="-ml-2.5 flex h-11 w-11 shrink-0 items-center justify-center">
      <svg
        width="22"
        height="22"
        viewBox="0 0 24 24"
        fill="none"
        stroke="currentColor"
        strokeWidth="1.8"
        strokeLinecap="round"
        strokeLinejoin="round"
        className="text-ink"
      >
        <path d="M15 18l-6-6 6-6" />
      </svg>
    </Link>
  );
}

/**
 * Header ripetuto in cima a ogni schermata: logo "Groci" sempre centrato
 * (24px, Caveat Brush) — a sinistra il titolo della pagina (Caveat,
 * ~30px) o, se la pagina ha un punto a cui tornare, solo una freccia
 * indietro al suo posto; a destra l'icona profilo. Tre colonne
 * `1fr auto 1fr`, non tre terzi uguali: così il logo resta
 * perfettamente centrato qualunque sia la lunghezza del titolo.
 */
export function PageHeader({
  token,
  title,
  backHref,
  subtitle,
}: {
  token: string;
  title: string;
  backHref?: string;
  subtitle?: string;
}) {
  return (
    <div className="flex flex-col gap-1 px-5 pb-2.5 pt-5">
      <div className="grid grid-cols-[1fr_auto_1fr] items-center">
        <div className="flex min-w-0 justify-start">
          {backHref ? (
            <BackButton href={backHref} />
          ) : (
            <h1 className="font-caveat min-w-0 truncate pr-2 text-[28px] font-bold leading-none text-ink">{title}</h1>
          )}
        </div>
        <Logo size={24} />
        <div className="flex justify-end">
          <ProfiloButton token={token} />
        </div>
      </div>
      {subtitle && <p className="text-sm text-ink/60">{subtitle}</p>}
    </div>
  );
}
