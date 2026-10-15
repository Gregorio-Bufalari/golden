import type { Metadata, Viewport } from "next";
import { Jost, Caveat, Caveat_Brush, Geist_Mono } from "next/font/google";
import "./globals.css";

const jost = Jost({
  variable: "--font-jost",
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
  themeColor: "#f3e8d9",
};

export default function RootLayout({ children }: LayoutProps<"/">) {
  return (
    <html
      lang="en"
      className={`${jost.variable} ${caveat.variable} ${caveatBrush.variable} ${geistMono.variable} h-full antialiased`}
    >
      <body className="min-h-full flex flex-col">{children}</body>
    </html>
  );
}
