import Image from "next/image";
import Link from "next/link";

function ProfiloButton({ token }: { token: string }) {
  return (
    <Link
      href={`/piano/${token}/impostazioni`}
      aria-label="Profilo"
      className="flex h-11 w-11 shrink-0 items-center justify-center rounded-full bg-panel print:hidden"
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

/**
 * Header ripetuto in cima a ogni schermata: il logo compare solo nel Menu
 * (la schermata "principale"), le altre hanno il semplice nome della
 * sezione — coerente con il piano di design approvato.
 */
export function PageHeader({
  token,
  title,
  logo = false,
  subtitle,
}: {
  token: string;
  title: string;
  logo?: boolean;
  subtitle?: string;
}) {
  return (
    <div className="flex flex-col gap-1 px-5 pb-2.5 pt-5">
      <div className="flex items-center justify-between">
        {logo ? (
          <Image src="/logo.png" alt="Groci" height={36} width={120} style={{ height: 36, width: "auto" }} priority />
        ) : (
          <h1 className="text-xl font-bold tracking-tight text-ink">{title}</h1>
        )}
        <ProfiloButton token={token} />
      </div>
      {subtitle && <p className="text-sm text-ink/60">{subtitle}</p>}
    </div>
  );
}
