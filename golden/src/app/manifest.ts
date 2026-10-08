import type { MetadataRoute } from "next";

// Necessario perché il service worker (richiesto dalle notifiche push, vedi
// public/sw.js) possa registrarsi come PWA — non introduce installabilità
// come obiettivo a sé, solo quanto serve perché il browser tratti l'app
// come un contesto valido per le notifiche.
export default function manifest(): MetadataRoute.Manifest {
  return {
    name: "Groci",
    short_name: "Groci",
    description: "Meal planning settimanale con attenzione a restrizioni alimentari e sprechi",
    start_url: "/",
    display: "standalone",
    background_color: "#fbfbf6",
    theme_color: "#425a30",
    icons: [
      {
        src: "/logo.png",
        sizes: "883x412",
        type: "image/png",
      },
    ],
  };
}
