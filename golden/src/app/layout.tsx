import type { Metadata, Viewport } from "next";
import { Jost, Fredoka, Caveat, Caveat_Brush, Geist_Mono } from "next/font/google";
import "./globals.css";

const jost = Jost({
  variable: "--font-jost",
  subsets: ["latin"],
});

// Solo per il wordmark "groci" (vedi components/logo.tsx) — mai per testo UI.
const fredoka = Fredoka({
  variable: "--font-fredoka",
  weight: "500",
  subsets: ["latin"],
});

const caveat = Caveat({
  variable: "--font-caveat",
  weight: "700",
  subsets: ["latin"],
});

const caveatBrush = Caveat_Brush({
  variable: "--font-caveat-brush",
  weight: "400",
  subsets: ["latin"],
});

const geistMono = Geist_Mono({
  variable: "--font-geist-mono",
  subsets: ["latin"],
});

export const metadata: Metadata = {
  title: "Groci",
  description: "Groci — Next.js app con Supabase",
};

export const viewport: Viewport = {
  themeColor: "#fbfaf6",
};

export default function RootLayout({ children }: LayoutProps<"/">) {
  return (
    <html
      lang="en"
      className={`${jost.variable} ${fredoka.variable} ${caveat.variable} ${caveatBrush.variable} ${geistMono.variable} h-full antialiased`}
    >
      <body className="min-h-full flex flex-col">{children}</body>
    </html>
  );
}
