"use client";

import Link from "next/link";
import { usePathname } from "next/navigation";

const TABS = [
  {
    href: "menu",
    label: "Menu",
    icon: (
      <>
        <rect x="5" y="3" width="14" height="18" rx="2" />
        <line x1="8" y1="8" x2="16" y2="8" />
        <line x1="8" y1="12" x2="16" y2="12" />
        <line x1="8" y1="16" x2="13" y2="16" />
      </>
    ),
  },
  {
    href: "spesa",
    label: "Spesa",
    icon: (
      <>
        <path d="M6 8h12l-1 12H7L6 8Z" />
        <path d="M9 8V6a3 3 0 0 1 6 0v2" />
      </>
    ),
  },
  {
    href: "frigo",
    label: "Frigo",
    icon: (
      <>
        <rect x="6" y="3" width="12" height="18" rx="1.5" />
        <line x1="6" y1="10" x2="18" y2="10" />
        <line x1="9" y1="5" x2="9" y2="8" />
        <line x1="9" y1="13" x2="9" y2="16" />
      </>
    ),
  },
  {
    href: "andamento",
    label: "Andamento",
    icon: (
      <>
        <line x1="6" y1="18" x2="6" y2="12" />
        <line x1="12" y1="18" x2="12" y2="7" />
        <line x1="18" y1="18" x2="18" y2="14" />
      </>
    ),
  },
  {
    href: "checkin",
    label: "Check-in",
    icon: (
      <>
        <circle cx="12" cy="12" r="8" />
        <path d="M8.5 12.5l2.3 2.3 4.7-5" />
      </>
    ),
  },
  {
    href: "preferiti",
    label: "Preferiti",
    icon: <path d="M12 20.5s-7.5-4.5-9.5-9A5 5 0 0 1 12 6a5 5 0 0 1 9.5 5.5c-2 4.5-9.5 9-9.5 9Z" />,
  },
];

// In basso, non in alto: pollice-raggiungibile camminando con il carrello.
export function BottomNav({ token }: { token: string }) {
  const pathname = usePathname();

  return (
    <nav
      className="sticky bottom-0 z-10 flex shrink-0 border-t border-ink/10 bg-paper px-1.5 pb-[calc(10px+env(safe-area-inset-bottom,0px))] pt-1.5 print:hidden"
      style={{ paddingBottom: "calc(10px + env(safe-area-inset-bottom, 0px))" }}
    >
      {TABS.map((tab) => {
        const href = `/piano/${token}/${tab.href}`;
        const attiva = pathname?.startsWith(href);
        return (
          <Link
            key={tab.href}
            href={href}
            aria-label={tab.label}
            className={`flex min-h-[52px] flex-1 flex-col items-center justify-center gap-[3px] ${
              attiva ? "text-accent" : "text-ink/55"
            }`}
          >
            <svg
              width="22"
              height="22"
              viewBox="0 0 24 24"
              fill="none"
              stroke="currentColor"
              strokeWidth="1.6"
              strokeLinecap="round"
              strokeLinejoin="round"
            >
              {tab.icon}
            </svg>
            <span className={`text-[11px] ${attiva ? "font-semibold" : "font-medium"}`}>
              {tab.label}
            </span>
          </Link>
        );
      })}
    </nav>
  );
}
