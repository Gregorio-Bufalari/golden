// Service worker minimo, solo per le notifiche push (nessuna cache offline
// — non è lo scopo di questa integrazione). Vedi src/lib/notifiche-scadenza.ts
// e /api/push/notifica-scadenze per cosa viene inviato.

self.addEventListener("install", () => {
  self.skipWaiting();
});

self.addEventListener("activate", (event) => {
  event.waitUntil(self.clients.claim());
});

self.addEventListener("push", (event) => {
  if (!event.data) return;

  let payload;
  try {
    payload = event.data.json();
  } catch {
    return;
  }

  const { titolo, corpo, url } = payload;
  event.waitUntil(
    self.registration.showNotification(titolo || "Groci", {
      body: corpo,
      icon: "/logo.png",
      data: { url: url || "/" },
    }),
  );
});

self.addEventListener("notificationclick", (event) => {
  event.notification.close();
  const url = event.notification.data?.url || "/";

  event.waitUntil(
    self.clients.matchAll({ type: "window", includeUncontrolled: true }).then((clientList) => {
      for (const client of clientList) {
        if (client.url.includes(url) && "focus" in client) {
          return client.focus();
        }
      }
      if (self.clients.openWindow) {
        return self.clients.openWindow(url);
      }
    }),
  );
});
