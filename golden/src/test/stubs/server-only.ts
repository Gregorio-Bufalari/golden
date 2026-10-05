// Stub per "server-only" nei test: quel pacchetto lancia sempre un errore
// se non viene sostituito a build-time dal bundler di Next.js (è solo un
// marcatore, non un controllo a runtime). Qui non serve: tutto il codice
// sotto test gira comunque lato server/Node, mai nel browser.
export {};
