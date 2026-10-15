# Groci — contesto tecnico per riprendere il lavoro

Documento di handoff per una nuova sessione Claude Code, nel caso questa esaurisca lo spazio di
contesto. Riassume stack, architettura, vincoli dell'ambiente e tutto ciò che è stato costruito,
in ordine. Aggiornalo (o riscrivilo) ogni volta che chiudi una sessione di lavoro significativa.

## Cos'è Groci

App di meal-planning settimanale per persone con restrizioni alimentari (nata per un caso
d'uso celiachia + riduzione sprechi). Nessun login: ogni utente ha un `link_token` unico
nell'URL (`/piano/[token]/...`) che identifica il suo profilo. L'AI (Claude) genera il piano
settimanale (pranzo+cena × 7 giorni), la lista della spesa e gestisce le modifiche in linguaggio
naturale; un livello di validazione statica (no AI) applica le garanzie di sicurezza dopo ogni
generazione.

## Stack

- **Next.js 16** (App Router, Turbopack), **React 19**, TypeScript, **Tailwind v4** (`@theme inline`
  in `src/app/globals.css` per i design token)
- **Supabase** Postgres, accesso solo via `createAdminClient()` (service role key, server-only,
  bypassa RLS) — niente login utente, l'unico "auth" è il `link_token`
- **Anthropic Claude** (`@anthropic-ai/sdk`, vedi `src/lib/claude.ts`) per generazione piano,
  modifiche in linguaggio naturale, adattamento budget
- **Vitest** per i test (`npm test`, mai chiamate reali) + uno smoke suite separato
  (`npm run test:smoke`, chiama DAVVERO l'API Anthropic, va eseguito a mano)
- Deploy su **Vercel**, auto-deploy da `master` + preview per ogni PR

Cartella del progetto: `/home/user/golden/golden` (nota: nel repo c'è una sottocartella `golden/`
— i path mostrati da `git diff --stat` cominciano con `golden/...`, ma il lavoro si fa dentro
`/home/user/golden/golden`, la working directory giusta).

## ⚠️ Vincolo d'ambiente critico: Supabase è irraggiungibile da questo sandbox

Il sandbox cloud in cui gira questa sessione **blocca in uscita l'host Supabase reale**
(`<project>.supabase.co` non è nell'allowlist di rete). Qualsiasi chiamata server-side a Supabase
(query, insert, admin client) fallisce con un errore di rete — sia da script ad-hoc sia dentro
`next dev`. Questo vuol dire:

- **Non puoi** avviare l'app normalmente e navigare le pagine reali (`/piano/[token]/...`) per
  verificarle: falliranno tutte al fetch dei dati.
- **Pattern di verifica usato finora**: crea una route temporanea (es. `src/app/previewtmp/page.tsx`)
  che importa il componente/la vista da verificare con **dati finti hardcoded** (nessuna chiamata
  Supabase), avvia `npm run dev`, scatta uno screenshot con Playwright
  (`npx --no-install playwright screenshot -b chromium --viewport-size "390,900" --wait-for-timeout 1000 "http://localhost:3000/previewtmp" "/tmp/out.png"`),
  guardalo col tool Read (legge immagini), **poi cancella sempre** la route temporanea e
  `.next/` prima di committare. Non lasciare mai `previewtmp` nel repo.
- L'**API Anthropic reale invece FUNZIONA** da questo sandbox (c'è `ANTHROPIC_API_KEY` in
  `.env.local`) — per verifiche che contano davvero (es. un cambio ai prompt di generazione),
  usa `npm run test:smoke` con le env var caricate: `set -a && source .env.local && set +a && npm run test:smoke`.
  Questi test costano token veri: usali con criterio, non ad ogni modifica minore.
- Un `next dev` lasciato acceso tra una richiesta e l'altra può sopravvivere come processo
  zombie e far puntare curl/Playwright a codice vecchio. Prima di una verifica nuova, assicurati
  che non ci siano processi `next-server`/`next dev` residui (`ps aux | grep next`), killali con
  `kill -9` se serve, e rigenera `.next/` da zero.

## Workflow Git/PR — IMPORTANTE

L'utente (Gregorio-Bufalari) **merge ogni PR quasi istantaneamente** dopo che la creo (spesso
entro pochi secondi). Questo significa:

1. Branch di lavoro: `claude/nextjs-supabase-setup-bmfb3e` (base `master`).
2. Prima di pushare qualunque commit nuovo, **controlla sempre** se l'ultima PR aperta su questo
   branch è già stata mergiata: `mcp__github__pull_request_read` method `get` sul numero di PR.
   Se è già `merged: true`, quella PR è chiusa per sempre — un nuovo commit sullo stesso branch
   **non vi entra più**.
3. Se la PR precedente è già mergiata, il branch locale/remoto va **riallineato a `master`**
   prima di continuare (vedi sotto), poi si apre una **PR nuova**.
4. Se invece la PR precedente è ancora aperta, i nuovi commit pushati sullo stesso branch
   confluiscono automaticamente in quella PR (nessuna nuova PR da aprire) — utile se l'utente
   dice esplicitamente di voler accumulare più feature in una sola PR prima di mergiare.
5. **Non pushare mai senza aver prima fatto lint + test + build in locale.**

### Come riallineare il branch dopo un merge

Dato che l'utente spesso fa "squash and merge" (o comunque la storia su `master` diverge
dall'identità dei commit del branch anche se il contenuto converge), un semplice `git push`
sul branch dopo un merge viene rifiutato (non fast-forward). La sequenza corretta:

```bash
git fetch origin master
git checkout -B claude/nextjs-supabase-setup-bmfb3e origin/master
# ... fai le modifiche, commit ...
git push -u origin claude/nextjs-supabase-setup-bmfb3e   # o --force-with-lease se serve
```

Questo è esplicitamente autorizzato quando il branch contiene solo storia già mergiata — non è
un'azione distruttiva in quel caso (non si perde nulla: tutto il contenuto è già su `master`).

### Formato PR

Ogni PR include nel corpo un **test plan** con lint/test/build/verifica visiva spuntati, e
termina con il footer di attribuzione Claude (vedi istruzioni di sistema della sessione per il
testo esatto — include un link alla sessione). Titoli brevi e descrittivi in italiano.

## Database — schema e migration

`supabase/schema.sql` è lo **schema canonico completo** (sempre tenuto aggiornato). La cartella
`supabase/migrations/` contiene però i singoli file incrementali, ciascuno con il commento
*"Esegui nel SQL Editor di Supabase sul database esistente"* — **queste migration NON girano in
automatico**, vanno eseguite a mano dall'utente sul progetto Supabase di produzione. Se aggiungi
una tabella: aggiorna `schema.sql` **e** crea un nuovo file in `migrations/` con la stessa
convenzione di nome (`YYYY-MM-DD_descrizione.sql`), e **ricorda esplicitamente all'utente** di
eseguirlo — un bug di questa sessione (crash della pagina Spesa) è nato proprio da una tabella
(`spesa_stato`) mai creata sul DB reale. Query a una tabella mancante di solito restituiscono un
errore invece di far crashare (supabase-js non lancia eccezioni sulle query), ma un **Server
Component** che prova a leggere dati durante il render SSR può comunque rompere l'intera pagina
se non gestito — vedi la sezione "Bug risolti" più sotto per il caso reale.

Tabelle principali (oltre a `profiles`, `weekly_plans`, `checkins` preesistenti):
- `rimanenze` — saldo "dispensa" (avanzi dalla spesa, riusati nei piani futuri)
- `spesa_stato` — spunte "acquistato" per riga della lista spesa
- `feedback_rapido` — feedback binario (pollice su/giù) + commento, uso interno
- `preferiti` — istantanea di un piatto salvato come preferito (nome, ingredienti, nutrizione,
  preparazione), **nessun collegamento al motore di generazione**

## Design system

Definito in `src/app/globals.css`. Palette **sempre chiara** (nessuna dark-mode: fu introdotta
per errore una volta e rimossa esplicitamente su richiesta utente — non reintrodurla mai senza
che sia esplicitamente nel piano approvato):

- `--paper` `#fbfbf6` sfondo di lettura
- `--panel` `#ecf3e0` pannelli/card/header/nav (sage, campionato dal logo)
- `--ink` `#1c1c16` testo (campionato dal logo)
- `--accent` `#425a30` (moss) — azione primaria, stato "selezionato/attivo" ovunque nell'app
- `--accent-fill-text` `#fbfbf6` testo su bottoni pieni accent
- `--clay` `#8c3b32` / `--clay-soft` — rischio/errore (raro, usato apposta)
- `--honey` `#a8742a` / `--honey-soft` — attenzione lieve

Principi: target di tocco minimi 44px ovunque; "profondità tramite colore, non ombra" (pannelli
`bg-panel`, non card con bordo/shadow); punto medio `·` come separatore nei metadati (mai
trattino lungo `—`); niente maiuscolo nelle etichette; niente frecce nei bottoni; nav in basso
(raggiungibile col pollice), mai in alto. Font: Geist Sans per UI, Geist Mono per numeri/prezzi/
quantità.

`src/components/form-kit.tsx` centralizza gli stili dei form (input, label, checkbox, pillole) —
usato sia da Onboarding sia da Impostazioni/Profilo per non farli divergere visivamente.

## Mappa dell'architettura

```
src/lib/claude.ts            — tutte le chiamate AI (generateMealPlan, modificaPiano,
                                adattaBudget, regeneratePasto, confrontaScontrino). Ogni
                                system prompt si compone con componiIstruzioni([...]) da
                                moduli condivisi (ISTRUZIONI_INGREDIENTI, ISTRUZIONE_VARIETA,
                                istruzioneDispensa()...) — la lista esplicita in ogni funzione
                                è la documentazione di cosa quella chiamata sa davvero, vedi
                                il commento sopra componiIstruzioni per la mappa completa.
src/lib/piano-validazione.ts — pipeline di validazione POST-generazione, nessuna chiamata AI
                                fuori da qui per la sicurezza: validaGiorni (controllo glutine
                                a 4 livelli, e in coda fonte_nutrizione per ogni pasto — vedi
                                nutrizione-crea.ts), assicuraVarieta (niente piatti duplicati
                                nella settimana), adattaEntroBudget (retry budget — chiama SEMPRE
                                assicuraVarieta anche internamente, sui giorni iniziali e dopo
                                ogni tentativo, così qualunque chiamante la ottiene gratis)
src/lib/allergeni-check.ts   — classificazione statica per ingrediente a 4 categorie, per 4
                                allergeni (glutine, frutta a guscio, uova, pesce/crostacei —
                                vedi sotto)
src/lib/grocery.ts           — costruzione lista della spesa da un piano validato
src/lib/dispensa.ts          — saldo "rimanenze" tra una settimana e l'altra
src/lib/conservazione.ts     — categoria di conservazione per ingrediente (Frigo)
src/lib/sprechi-evitati.ts   — stima € "sprechi evitati" (Andamento)
src/lib/varieta-giorno.ts    — controllo leggero post-scambio pasti (stesso ingrediente
                                "principale" a pranzo e cena dello stesso giorno?)
src/lib/settimana.ts         — data reale di un giorno della settimana, solo per display
src/lib/quantita.ts          — formattazione quantità per porzione, scalata su household_size
src/lib/scadenza-frigo.ts    — data di scadenza stimata per voce del Frigo (euristica per
                                categoria ingrediente) + giorni mancanti, per il banner.
                                coloreUrgenza(giorni) decide il colore del pallino dai giorni
                                REALI rimanenti (non più dalla sola categoria, vedi conservazione.ts)
src/lib/nutrizione-crea.ts   — tabella statica valori CREA/INRAN (per 100g) + calcolaNutrizionePasto:
                                sostituisce la stima AI col dato ufficiale quando TUTTI gli
                                ingredienti del pasto sono coperti ed espressi in g/kg, mai
                                mischiato nello stesso piatto. Applicato da piano-validazione.ts
                                (applicaFonteNutrizione, in coda a validaGiorni), non un passaggio
                                a parte — vedi "Nutrizione: CREA/INRAN quando copre tutto il
                                piatto..." in Pattern ricorrenti.
src/lib/calibrazione-prezzi.ts — "learning loop" prezzi: fattore correttivo derivato dai
                                check-in passati (spesa_reale vs budget_stimato) PER
                                SUPERMERCATO, applicato sopra la fascia statica in grocery.ts.
                                calcolaFattoreCalibrazionePerProfilo() centralizza la lettura
                                Supabase, riusata da generate/modifica/conferma — non duplicarla.
src/lib/preferiti-scoperta.ts — in modalità Scoperta, sceglie se e quale Preferito includere
                                nel piano di questa settimana (a rotazione, mai lo stesso finché
                                ce n'è un altro in attesa) e lo sostituisce nel piano generato.
                                includiFavoritoNelPiano() va chiamata DOPO adattaEntroBudget
                                (non prima): l'AI che riduce il costo rivede liberamente tutti i
                                pasti e potrebbe alterare il Preferito se fosse già presente.
src/lib/notifiche-scadenza.ts — cosa notificare per le scadenze Frigo (ingredientiInScadenzaDomani),
                                condiviso tra il banner in-app di fallback e la notifica push vera,
                                così i due canali concordano sempre.
src/lib/web-push.ts          — invio di una notifica push via VAPID (richiede VAPID_PUBLIC_KEY/
                                VAPID_PRIVATE_KEY in env); rimuove da sola una sottoscrizione che
                                il servizio push segnala come non più valida (404/410).
src/lib/costo-ai.ts          — calcolaCostoUsd(usage, model): stima in USD di UNA chiamata a
                                Claude dai token in response.usage, tabella prezzi solo per i
                                modelli usati da questa app — aggiorna se cambia ANTHROPIC_MODEL.

src/app/api/piano/generate/route.ts  — genera un piano nuovo (o riusa l'ultimo in modalità
                                        "routine"). Se c'è un piano routine riusabile, salva
                                        subito come prima; altrimenti genera TRE scenari a
                                        budget diverso (vedi "Scenari multipli di budget" più
                                        sotto) e li restituisce SENZA salvarli — li salva solo
                                        /api/piano/conferma, quando l'utente ne sceglie uno.
src/app/api/piano/conferma/route.ts  — salva lo scenario scelto dall'utente: riceve dal client
                                        solo `giorni`, ricalcola SEMPRE lato server lista della
                                        spesa e consumi dispensa (mai fidarsi di quelli mostrati
                                        nel confronto) e ri-passa da validaGiorni — prima volta
                                        in cui un piano arriva "echeggiato" dal client invece che
                                        da Claude, quindi prima volta in cui si rivalida.
src/app/api/piano/modifica/route.ts  — UNICO endpoint per ogni modifica in linguaggio naturale
                                        al piano: "Proponi un piatto diverso", +/- nutrienti,
                                        "Sostituisci"/"Non l'ho trovato", ecc. Passa sempre da
                                        validaGiorni — nessuna validazione duplicata altrove.
                                        Legge/scrive modifica_messaggi: le ultime 5 richieste su
                                        QUESTO piano (memoria conversazionale, vedi
                                        testoCronologia in claude.ts) e il costo stimato di ogni
                                        chiamata (anche loggato in console da modificaPiano).
src/app/api/push/notifica-scadenze/route.ts — invocato una volta al giorno da Vercel Cron (vedi
                                        vercel.json): per ogni profilo con almeno una notifica
                                        push attiva, controlla cosa scade domani e invia la
                                        notifica a tutti i suoi dispositivi sottoscritti.
src/app/api/scontrino/estrai/route.ts — legge la foto di uno scontrino (vision, nessun OCR
                                        esterno) e la confronta con la grocery_list dell'ultimo
                                        piano del profilo. Versione semplice: elaborazione al
                                        volo, l'immagine non viene mai salvata né persistita.

src/app/piano/[token]/archivio/      — Archivio (nuovo, sola lettura): elenco delle settimane
  page.tsx, [settimana]/page.tsx       passate (una riga `weekly_plans` per settimana, nessuna
                                        tabella nuova) e dettaglio di una settimana scelta, con
                                        le SUE date reali (mai quelle di oggi). Nessuna
                                        interazione: niente scambio pasti, niente chat di
                                        modifica — solo consultazione. Raggiungibile da un link
                                        "Settimane passate" in Profilo (Impostazioni).

src/app/piano/[token]/
  layout.tsx, bottom-nav.tsx, page-header.tsx  — shell condivisa (6 tab: Menu, Spesa, Frigo,
                                                   Andamento, Check-in, Preferiti)
  menu/                — schermata principale: piano settimanale, scambio pasti, preferiti,
                          confronto nutrizionale LARN, box "Modifica il piano" (NL)
  spesa/                — lista della spesa, spunte, "Non l'ho trovato"
  frigo/                — dispensa residua con pallino colorato per urgenza di consumo; banner
                           di scadenza in-app SOLO se il profilo non ha notifiche push attive
                           (notifiche-push.tsx propone di attivarle, solo quando c'è già
                           qualcosa da notificare — mai un prompt proattivo al primo avvio)
  andamento/            — KPI: risparmio vs budget, % settimane senza sprechi, sprechi evitati €
  checkin-form.tsx      — check-in settimanale (seguito il piano? sprecato? spesa reale?) +
                           "Fotografa lo scontrino" (comprime l'immagine lato client prima di
                           inviarla, vedi comprimiImmagine) che confronta coi prezzi stimati e
                           precompila "quanto hai speso" col totale letto
  impostazioni/         — Profilo: riepilogo in sola lettura + pulsante Modifica in fondo
  preferiti/            — lista consultabile dei piatti salvati come preferiti
  push-actions.ts       — server action salvaPushSubscription()/rimuoviPushSubscription()
  actions.ts            — server action scambiaPasti() + setModalita()

src/components/supermercato-selector.tsx — componente condiviso di scelta supermercato,
  riusato in Onboarding, Profilo (Impostazioni) e Check-in: stessa lista
  (SUPERMERCATO_OPTIONS) e stesso stile pillola ovunque, nessuna duplicazione.

src/lib/settimana.ts — anche la chiave stabile del giorno nel Menu/Archivio: etichettaGiorno()
  deriva SEMPRE la data reale dalla `settimana` della riga weekly_plans in questione (mai da
  "oggi"), mai dall'ordine/contenuto dei pasti — vedi "Giorno identificato da nome fisso + data
  derivata, mai da un indice" più sotto.

public/sw.js — service worker minimo, solo per le notifiche push (nessuna cache offline).
src/app/manifest.ts — web manifest, richiesto perché il service worker possa registrarsi.
```

## Pattern ricorrenti da rispettare

- **Validazione di sicurezza centralizzata**: `validaGiorni` in `piano-validazione.ts` è l'UNICO
  punto che applica la sicurezza glutine. Ogni nuova funzionalità che tocca il piano deve
  richiamarlo, mai duplicare la logica.
- **Server action, non API route**, per le mutazioni semplici legate a un token (vedi
  `actions.ts`, `checkin-actions.ts`, `feedback-actions.ts`, `preferiti-actions.ts`): pattern
  `"use server"` → `createAdminClient()` → lookup `profiles` by `link_token` → query. Non fidarsi
  mai di un id mandato dal client quando si può ricavare dal token.
  Un esempio di questo pattern applicato alla sicurezza: lo scambio pasti (`scambiaPasti`) riceve
  dal client solo le **coordinate** (giorno/indice), mai il contenuto del pasto — lo scambio vero
  avviene lato server sui dati già salvati, così il client non può mai iniettare contenuto nuovo.
- **Aggiornamento ottimistico** per le azioni rapide (spunte spesa, preferiti): aggiorna lo stato
  locale subito, chiama il server action, fai rollback se torna un errore.
- **Nessuna influenza sul motore AI senza che sia esplicitamente richiesto.** "Preferiti" è nato
  deliberatamente come sola lettura, nessun collegamento al motore ("versione semplice" richiesta
  esplicitamente) — poi esteso, su richiesta esplicita successiva, SOLO in modalità Scoperta (vedi
  "Preferiti che influenzano Scoperta" più sotto): il principio resta che nessuna funzionalità
  tocca il motore senza che sia stato chiesto, non che Preferiti sia per sempre sola lettura.
- **Splice deterministico, non istruzione all'AI, quando serve riusare un dato esatto già noto**:
  quando l'obiettivo è includere QUALCOSA DI GIÀ NOTO per intero (es. un piatto salvato nei
  Preferiti, istantanea completa di ingredienti/nutrizione/preparazione), si sostituisce
  direttamente nel piano generato invece di chiedere all'AI di "includere questo piatto" nel
  prompt — più affidabile, e il pasto resta esattamente quello salvato, non una reinterpretazione
  del modello. Va fatto DOPO `adattaEntroBudget`, non prima: l'AI che riduce il costo rivede
  liberamente tutti i pasti e potrebbe alterare anche questo se fosse già presente in quel
  passaggio (bug corretto nella sessione "Scenari multipli di budget" — il codice precedente lo
  inseriva prima). Dopo lo splice va ripetuto `validaGiorni` (il pasto appena inserito potrebbe
  non essere mai stato controllato con le restrizioni ATTUALI) e ricalcolato `buildGroceryList`
  (il suo costo non ha partecipato all'ottimizzazione budget). Vedi `includiFavoritoNelPiano` in
  `preferiti-scoperta.ts` e come viene usata in `generate/route.ts`.
- **Stime esplicitamente etichettate come tali** nella UI quando non sono dati precisi (budget
  stimato, confronto LARN, sprechi evitati €) — mai presentare una stima come un dato esatto.
- **Supermercato: due significati diversi, mai confusi.** In Onboarding/Profilo, `profile.supermercato`
  è solo il riferimento per tarare la fascia prezzo statica (`fasciaDaSupermercato` in grocery.ts) —
  non raggiunge mai il prompt AI (vedi `buildContestoProfilo` in claude.ts). Nel Check-in,
  `retailer_usato` è il negozio **davvero** usato quella settimana e può differire dal riferimento:
  alimenta `fattoreCalibrazione` (calibrazione-prezzi.ts), che confronta `spesa_reale`/`budget_stimato`
  solo sui check-in dove il retailer coincide col supermercato di riferimento interrogato — mai mescolare
  dati di retailer diversi nella stessa media.
- **"Learning loop" derivato, non AI**: ogni correzione imparata dai dati storici (es. il fattore di
  calibrazione prezzi) è una funzione pura e deterministica su dati già raccolti, con soglia minima di
  campioni e range di clamping per non farsi distorcere da un singolo valore anomalo — stesso spirito
  di `sprechi-evitati.ts` e `varieta-giorno.ts`, niente nuove chiamate Claude per queste stime.
- **Generare ≠ salvare, quando l'utente deve prima scegliere.** Introdotto con "Scenari multipli di
  budget": `/api/piano/generate` può restituire più proposte SENZA scrivere nulla (niente
  `weekly_plans`, niente consumo dispensa, niente rotazione Preferiti) — qualunque effetto
  collaterale di una generazione che potrebbe essere scartata va rimandato a un secondo endpoint
  dedicato (`/api/piano/conferma`), chiamato solo quando l'utente conferma davvero. Quel secondo
  endpoint è anche la prima volta che un piano arriva "echeggiato" dal client invece che generato
  da Claude in quella stessa richiesta: non fidarsi dei valori derivati (lista della spesa, consumi
  dispensa) che il client rimanda indietro, RICALCOLARLI sempre lato server da `giorni` (validato
  con `MealPlanSchema.safeParse` prima di tutto) — fidarsi solo del contenuto dei pasti in sé, già
  scelto dall'utente tra alternative mostrate, mai modificato a mano.
- **Permesso del browser (notifiche, geolocalizzazione, ecc.) richiesto solo al primo utilizzo
  utile, mai proattivamente.** `notifiche-push.tsx` è montato dal chiamante SOLO quando c'è già
  qualcosa da notificare (vedi `frigo/page.tsx`) — non all'avvio dell'app — e al suo interno
  chiama `Notification.requestPermission()` SOLO in risposta a un click esplicito su "Attiva
  notifiche", mai automaticamente in un effect al mount (i browser lo scoraggiano comunque). Se il
  permesso è già `"denied"`, non mostra nulla; se è già `"granted"` (dato in una sessione
  precedente), si sottoscrive silenziosamente senza richiederlo di nuovo.
- **Canale push attivo sostituisce il banner in-app corrispondente, non lo affianca.** Il banner
  di fallback (`frigo/page.tsx`) si mostra solo se il profilo non ha nessuna `push_subscriptions`
  attiva — altrimenti l'utente viene avvisato comunque, anche senza aprire l'app, e il banner
  ridondante sparisce. Controllo lato server (conta le sottoscrizioni), non con `localStorage`: se
  l'utente ha attivato le notifiche su un dispositivo, il banner sparisce ovunque, non solo lì.
- **Memoria conversazionale limitata, non l'intera cronologia.** `modificaPiano` riceve al
  massimo le ultime `CRONOLOGIA_MAX` (5) richieste precedenti su quel piano come testo breve
  ("richiesta" → "applicata"/"rifiutata"), non l'intero scambio o le versioni passate del piano —
  basta per risolvere un riferimento implicito ("anche lì", "idem per cena") senza far crescere il
  costo per chiamata in modo illimitato man mano che la conversazione si allunga. Il limite è una
  costante nella route (`/api/piano/modifica`), non nel prompt stesso.
- **Costo per chiamata monitorato alla fonte, non ricostruito altrove.** `calcolaCostoUsd` (in
  `costo-ai.ts`) legge `response.usage` — già restituito dalla stessa chiamata, nessuna chiamata
  in più per saperlo — e il risultato si logga in console nello stesso momento in cui la chiamata
  avviene, non ricalcolato in un passaggio successivo. Applicato a `modificaPiano` (anche salvato
  su riga, vedi `modifica_messaggi.costo_stimato_usd`) e a `confrontaScontrino`; `generateMealPlan`,
  `adattaBudget` e `regeneratePasto` non hanno ancora questo monitoraggio — estendilo se richiesto
  esplicitamente, stesso principio delle altre funzionalità "non attive finché non è chiesto".
- **OCR via vision di Claude, mai un servizio OCR esterno.** `confrontaScontrino` manda la foto
  dello scontrino direttamente come content block `image` nella stessa chiamata che fa anche il
  confronto semantico con la lista della spesa (nomi abbreviati/diversi, es. "POLLO PETTO" vs
  "Petto di pollo" — un matching per stringa esatta fallirebbe quasi sempre su scontrini reali).
  Nessuna persistenza: l'immagine e il risultato del confronto esistono solo per la singola
  richiesta, coerente con "versione semplice" per una prima iterazione di una funzionalità nuova
  (stesso approccio usato per Preferiti all'inizio). Il client ridimensiona/ricomprime sempre la
  foto prima di inviarla (`comprimiImmagine` in `checkin-form.tsx`, max 1500px, JPEG qualità 0.8)
  — sia per restare sotto i limiti di corpo richiesta sia perché i token di visione scalano con
  la risoluzione dell'immagine.
- **Prompt composti da moduli dichiarati esplicitamente, stesso meccanismo one-shot di sempre.**
  `componiIstruzioni([...])` in `claude.ts` sostituisce la concatenazione a mano (`A + " " + B +
  " " + C`) con una lista esplicita per funzione — decisione presa dopo che l'uso reale di questa
  sessione ha reso evidente quali moduli servono dove (es. `ISTRUZIONI_INGREDIENTI`/`NUTRIZIONE`/
  `PREPARAZIONE` sempre, `ISTRUZIONE_VARIETA`/`BATCH_COOKING` solo nelle chiamate che generano/
  rivedono l'intero piano). **Non** è stato introdotto il tool-calling reale dell'API Anthropic
  (nessun loop multi-turno, nessuna chiamata agentica a strumenti): resta una singola chiamata con
  output strutturato per funzione, identica affidabilità/latenza di prima — scelta deliberata,
  discussa con l'utente prima di procedere, per non rischiare un motore già affidabile e testato.
  Se in futuro serve davvero il tool-calling reale (Claude che decide a runtime quali capacità
  invocare), è una richiesta esplicita a parte, non implicita in "tool-calling completo".
- **Un controllo deterministico va centralizzato dove viene DAVVERO applicato sempre, non
  ripetuto ad ogni chiamante.** Bug corretto in questa sessione: `assicuraVarieta` veniva invocata
  esplicitamente solo dalla route di generazione, MAI dalla route di modifica né dopo un retry
  budget di `adattaBudget` — un piano rivisto per il costo, o una singola modifica, potevano
  reintrodurre un doppione senza che nulla lo intercettasse. Fix: spostata DENTRO
  `adattaEntroBudget` stessa (chiamata sui giorni iniziali e dopo ogni tentativo budget), così
  ogni chiamante la ottiene automaticamente continuando a chiamare solo `adattaEntroBudget` come
  già faceva — nessuna route ha dovuto aggiungere una chiamata esplicita in più.
- **Giorno identificato da nome fisso + data derivata, mai da un indice.** `giorno.giorno`
  (`"Lunedì"`...`"Domenica"`, enum fisso in `GiornoSchema`) è e resta la chiave usata per scambio
  pasti, riferimenti in chat e validazione — NON una data persistita sul singolo giorno. La data
  reale mostrata in UI (`etichettaGiorno` in `settimana.ts`, es. "Gio 15 ott") è sempre *derivata*
  da `(giorno.giorno, settimana)` al momento del render, mai salvata: così resta automaticamente
  stabile sotto scambio pasti (che riassegna solo `pasti[indice]`, mai `giorno.giorno` — vedi
  `scambiaPasti` in `actions.ts`) e sotto una modifica che sostituisce il contenuto di un giorno
  (lo schema impone sempre esattamente 7 giorni con nomi fissi, quindi nessuno può scalare o
  sparire). Per una settimana passata (Archivio), la stessa funzione riceve la `settimana` di
  QUELLA riga `weekly_plans`, mai quella corrente — nessun calcolo rispetto a "oggi". Verificato
  con test espliciti in `menu-view.test.ts` (scambio, sostituzione contenuto) e
  `settimana.test.ts` (settimana passata vs corrente).
- **Nutrizione: CREA/INRAN quando copre TUTTO il piatto, mai mischiata con la stima AI.**
  `nutrizione-crea.ts` ha una tabella statica (fornita dal founder, nessuna chiamata esterna) di
  valori ufficiali per 100g. `calcolaNutrizionePasto` — chiamata da `applicaFonteNutrizione` in
  coda a `validaGiorni`, stesso principio di centralizzazione di `assicuraVarieta` — sostituisce
  `pasto.nutrizione` col dato CREA scalato sulla quantità reale SOLO se ogni ingrediente del
  piatto è sia coperto dalla tabella sia espresso in g/kg (ml/l/pz/confezione non sono
  convertibili senza una densità non fornita); altrimenti l'intero piatto resta la stima AI
  originale, mai un mix dei due. Il match sul nome ingrediente è per **parola intera**, non per
  sottostringa (`cercaValoriCrea`): "fagioli" non deve combaciare con "fagiolini", "mela" non con
  "melanzana" — un rischio concreto con la sottostringa, visto che `"fagiolini".includes("fagioli")
  === true`. Alcune voci hanno anche `esclusioni` (es. "latte di mandorla"/"yogurt greco"/"patate
  dolci"/"burro di arachidi") per non attribuire il profilo nutrizionale di un prodotto animale/
  base a una variante vegetale o a un prodotto diverso che condivide la stessa parola generica.
  L'Aglio è volutamente escluso dalla tabella (valore trovato non attendibile, richiesta esplicita
  del founder) — resta sempre stima AI. In UI, la fonte è mostrata ESPLICITAMENTE accanto ai
  valori di ogni piatto (Menu, Archivio, Preferiti), mai solo in un disclaimer generale: etichetta
  "Fonte: CREA/INRAN - Centro di ricerca Alimenti e Nutrizione" (citazione obbligatoria, non
  facoltativa — `CITAZIONE_CREA`) o "Stima AI". I Preferiti salvano una propria colonna
  `fonte_nutrizione` nello snapshot (nuova migration) — indipendente dal piano live, che la
  ricalcola sempre fresca ogni volta che passa da `validaGiorni`.

## Validazione sicurezza allergeni — a 4 livelli, 4 allergeni (generalizzata di recente)

`src/lib/allergeni-check.ts` (prima `glutine-check.ts`, rinominato quando generalizzato — vedi
voce cronologica dedicata): `categorizzaIngrediente(nome, allergeni)` restituisce una di, per
ciascuno degli allergeni attivi per il profilo (unione per sicurezza: basta che un ingrediente sia
rischioso per UNO degli allergeni richiesti):
- `"verificato"` — certificazione esplicita ("senza glutine", "vegan", "certificato")
- `"informazioni_sufficienti"` — nessun segnale di rischio, o sicuro per natura (es. "pasta di riso")
- `"da_verificare"` — dipende da marca/formulazione (es. dado vegetale/salsa di soia per il
  glutine, pesto/cioccolato per la frutta a guscio, pasta fresca/pane per le uova, salsa worcester
  per pesce/crostacei) — **non blocca** il pasto, solo segnalato per controllo etichetta
- `"non_adatto"` — rischio senza ambiguità (es. farina di frumento/pane/pasta per il glutine,
  mandorle/noci/arachidi per la frutta a guscio, uova/maionese per le uova, salmone/gamberi/cozze
  per pesce e crostacei) — **blocca** il pasto, fa scattare un tentativo di rigenerazione AI
  (`regeneratePasto`, max 2 tentativi, generico: non menziona mai un allergene specifico nel
  prompt, riceve solo l'elenco di nomi ingrediente da evitare)

Allergeni gestiti con questo controllo deterministico: glutine (celiachia), frutta a guscio, uova,
pesce/crostacei — mappati dalle esatte stringhe di `profiles.restrizioni` tramite
`allergeniDalProfilo()`. Le altre restrizioni selezionabili in onboarding (Lattosio, Vegetariano,
Vegano) **non hanno ancora** un controllo deterministico: restano affidate solo al prompt AI,
esattamente come tutte e quattro erano prima di questa generalizzazione.

Il banner nel Menu ("Verifica necessaria" di una volta) distingue rosso/clay ("Non adatto",
rigenerazione fallita) da ambra/honey ("Da verificare", solo da controllare in etichetta) — stesso
comportamento di prima, ora alimentato da più allergeni insieme.

## Ordine cronologico di cosa è stato costruito (PR #16 → #54, tutte mergiate)

Le PR più vecchie (16-31) sono di una sessione precedente: setup iniziale, generazione piano,
fix vari, lista spesa con "Non l'ho trovato"/"Proponine un altro", stagionalità, dispensa.

Questa sessione (dalla PR #32 in poi), in ordine:
1. **#32-35**: rinominato Golden → Groci, redesign completo (palette/nav/6 schermate), fix
   dark-mode mai approvata, allineamento Onboarding al design system
2. **#36-37**: fix larghezza card Menu disallineata, rimosso prezzo dalla card pasto, **fix
   crash Spesa** (causa reale: `ingredientiARischioSettimana` esportata da un modulo `"use
   client"` e chiamata direttamente da un Server Component — errore classico RSC, non un
   problema di dati mancanti come sembrava inizialmente)
3. **#38**: pallino colorato Frigo per urgenza di consumo (rosso/arancione/verde)
4. **#39**: **fix varietà piano generato** — "Ridurre gli sprechi" come obiettivo spingeva l'AI a
   ripetere pochi piatti; aggiunta `ISTRUZIONE_VARIETA` nei prompt + `assicuraVarieta()` come
   controllo deterministico post-generazione
5. **#40**: Profilo come riepilogo in sola lettura + pulsante Modifica
6. **#41**: scambio pasti tra giorni (prima versione, scambio immediato al secondo tocco)
7. **#42**: date reali sulle intestazioni dei giorni nel Menu (solo display, l'identificatore
   interno resta il nome del giorno)
8. **#43**: KPI "Sprechi evitati" in € in Andamento (stima da Frigo + check-in)
9. **#44**: Modifica spostato in fondo al riepilogo Profilo; scambio pasti reso a 3 fasi (arma →
   proponi → conferma esplicita, con controllo conflitti di varietà e suggerimento di un giorno
   alternativo prima di applicare)
10. **#45**: Preferiti (icona cuore, vista dedicata, versione semplice) + **validazione
    sicurezza a 4 livelli** (vedi sopra)
11. **#46**: `HANDOFF.md` iniziale (questo documento)
12. **#47**: dettaglio ingredienti/quantità per singola porzione (scalato su household_size,
    pulsante "Preparazione" per piatto) + priorità batch cooking nel motore (a parità di altre
    priorità, preferisce combinazioni di piatti che condividono ingredienti principali, per
    ridurre costo/spreco)
13. **#48**: data di scadenza stimata per voce del Frigo (`scadenza-frigo.ts`) + banner un
    giorno prima della scadenza
14. **#49**: comportamento del selettore supermercato chiarito/completato — in Onboarding/Profilo
    resta solo riferimento per tarare le stime prezzo (già così, verificato); nel Check-in il
    retailer dichiarato alimenta un vero **learning loop** (`calibrazione-prezzi.ts`, nuovo) che
    corregge le stime prezzo nel tempo in base allo scostamento storico spesa_reale/budget_stimato
    per quel supermercato; componente di selezione unificato (`SupermercatoSelector`) riusato in
    tutti e tre i punti al posto di liste/stili duplicati
15. **#50**: **Obiettivi nutrizionali per pasto** in Profilo (nuova sezione:
    calorie min/max, proteine minime, carboidrati/grassi massimi per pasto, tutti opzionali) —
    diversi dal confronto LARN (automatico, dai dati biometrici, solo informativo): questi sono
    un target esplicito passato al motore come vincolo aggiuntivo, **stessa priorità
    dell'obiettivo generale**, sempre sotto restrizioni alimentari e budget (vedi
    `obiettiviNutrizionaliTesto` in `claude.ts`). Non aggiunto in Onboarding (richiesto solo per
    Profilo), nessuna validazione post-generazione (a differenza del glutine): è un'istruzione nel
    prompt, non un vincolo rigido verificato dopo
16. **#51**: **Preferiti che influenzano Scoperta** — in modalità Scoperta, circa una settimana su
    tre include un piatto dai Preferiti invece di puntare solo a varietà pura
    (`sceglieFavoritoScoperta` in `preferiti-scoperta.ts`), scegliendo sempre quello riproposto
    meno di recente (rotazione, colonna `preferiti.ultima_proposta`) così nessuno si ripete finché
    ce n'è un altro in attesa. Il piatto scelto viene sostituito direttamente nel piano generato
    (non chiesto all'AI nel prompt), così passa dalla stessa sicurezza/varietà di ogni altro
    pasto. Prima eccezione al principio "Preferiti è sola lettura" (vedi Pattern ricorrenti) —
    resta vero che nessuna funzionalità tocca il motore senza che sia stato chiesto esplicitamente
17. **#52**: **Scenari multipli di budget** — `/api/piano/generate`, quando genera
    fresco (non riusa un piano routine), produce TRE scenari ("Risparmio" -20%, "Equilibrato",
    "Più abbondante" +20%, percentuali relative al costo EFFETTIVO dell'equilibrato, non al budget
    nominale) invece di salvare subito un piano solo — ciascuno è un piano generato da zero per
    quel target (piatti/ingredienti diversi, non lo stesso piano ridimensionato), con lo stesso
    eventuale Preferito (Scoperta) per restare confrontabili. Introduce lo split generare/salvare
    (vedi Pattern ricorrenti) e il nuovo `/api/piano/conferma`, che salva solo lo scenario scelto
    dall'utente in Menu (nuova schermata di confronto in `menu-view.tsx`, con "Annulla, mantieni
    il piano attuale" se c'era già un piano). Nello stesso lavoro, **corretto un bug** nella
    feature precedente: `includiFavoritoNelPiano` veniva chiamata PRIMA di `adattaEntroBudget`,
    rischiando che l'AI del retry budget alterasse il Preferito appena inserito — ora va dopo
    (vedi Pattern ricorrenti). `calcolaFattoreCalibrazionePerProfilo` estratta in
    `calibrazione-prezzi.ts`, centralizzando una lettura Supabase finora duplicata in due route
18. **#53**: **Notifiche push vere** — sostituiscono (quando attive) il banner
    in-app di scadenza Frigo. Nuova tabella `push_subscriptions`, nuovo service worker minimo
    (`public/sw.js`, solo push — nessuna cache offline) e manifest (`src/app/manifest.ts`,
    richiesto perché il SW possa registrarsi). `notifiche-push.tsx` propone di attivarle SOLO
    quando c'è già qualcosa da notificare (mai un prompt proattivo all'avvio) e richiede il
    permesso SOLO al click esplicito su "Attiva" (mai in automatico in un effect). Un nuovo
    endpoint cron (`/api/push/notifica-scadenze`, invocato una volta al giorno da Vercel Cron —
    vedi `vercel.json`) controlla ogni profilo sottoscritto e invia la notifica via `web-push`
    (libreria nuova) con chiavi VAPID. **Richiede setup manuale dell'utente**: eseguire la
    migration, generare una coppia di chiavi VAPID (`npx web-push generate-vapid-keys`, già fatto
    una volta in questa sessione — le chiavi sono nel messaggio di chiusura), impostarle come env
    var sia in locale sia su Vercel (vedi `.env.example`), e impostare `CRON_SECRET` (qualunque
    stringa casuale) su Vercel perché il cron possa autenticarsi — senza queste variabili il cron
    fallisce silenziosamente (nessun invio, nessun errore visibile all'utente finale, solo nei log
    Vercel). Non testabile end-to-end in questo sandbox (Supabase irraggiungibile): verificato il
    flusso fino alla chiamata del server action incluso, vedi commit per i dettagli.
19. **#54**: **Chat con memoria conversazionale** — `modificaPiano` riceve ora le
    ultime 5 richieste di modifica fatte su QUESTO piano (testo + esito, non il piano intero),
    per capire riferimenti impliciti come "anche lì" o "idem per cena" (`testoCronologia` in
    `claude.ts`, limite in `/api/piano/modifica/route.ts` — vedi Pattern ricorrenti). Nuova
    tabella `modifica_messaggi` (weekly_plan_id, messaggio, esito, token, costo) sia per questa
    memoria sia per il monitoraggio costo: ogni chiamata a `modificaPiano` ora calcola e
    restituisce `costoStimatoUsd` (`src/lib/costo-ai.ts`, prezzi $/1M token per modello — solo
    `claude-sonnet-5-5`/`claude-opus-5-5`, aggiorna la tabella se cambia `ANTHROPIC_MODEL`),
    loggato in console e salvato su riga.
20. **(questa sessione)**: **Foto scontrino + OCR** — nel Check-in, "Fotografa lo scontrino"
    (`checkin-form.tsx`) legge la foto via vision di Claude (nessun servizio OCR esterno, vedi
    Pattern ricorrenti) e la confronta semanticamente con la `grocery_list` dell'ultimo piano
    (`confrontaScontrino` in `claude.ts`, nuovo endpoint `/api/scontrino/estrai`): per ogni
    articolo della lista dice se è stato trovato sullo scontrino e a che prezzo, più gli eventuali
    prodotti extra non previsti. Il totale letto precompila "quanto hai speso davvero" (resta
    modificabile). Versione semplice, nessuna persistenza: immagine e risultato esistono solo per
    la singola richiesta. Stesso monitoraggio costo di `modificaPiano` (vedi sopra), non salvato
    su riga in questo caso (nessuna tabella dedicata, solo log console). Nella stessa sessione,
    aggiunte a `BACKLOG.md` due note dal founder non ancora implementate: **Mappa supermercati**
    (se costruita, va limitata a supermercati con assortimento senza glutine confermato dagli
    utenti nel tempo) e **Condivisione piano famiglia** (esplicitamente bloccata finché l'app non
    ha un vero sistema di login) — nessuna delle due è stata costruita, solo documentate come
    vincoli/blocchi per quando (se) verranno affrontate.
21. **(questa sessione)**: **Motore AI: prompt composti da moduli espliciti + fix varietà** —
    quarta richiesta del batch ("tool-calling completo"), chiarita in chat: NON tool-calling reale
    dell'API Anthropic (nessun loop multi-turno), ma modularizzazione della composizione del
    prompt. Nuovo `componiIstruzioni([...])` in `claude.ts` sostituisce la concatenazione a mano;
    ogni funzione (`generateMealPlan`, `modificaPiano`, `adattaBudget`, `regeneratePasto`)
    dichiara esplicitamente i propri moduli (vedi Pattern ricorrenti sopra per l'elenco). Bug
    corretto nel farlo: `modificaPiano` non applicava mai `assicuraVarieta`, a differenza di
    `generateMealPlan` — risolto centralizzando il controllo DENTRO `adattaEntroBudget` (sui
    giorni iniziali e dopo ogni retry budget), correggendo anche un gap analogo non notato nel
    flusso di generazione stesso. Nessuna route ha dovuto cambiare tranne `generate/route.ts`
    (rimossa la chiamata a `assicuraVarieta` ormai ridondante). Stessa PR #55 della voce
    precedente (foto scontrino), unita perché pushata prima del merge.
22. **(questa sessione)**: **Giorni del Menu identificati per data reale, non per indice** +
    **Archivio (nuovo)**. Verificato prima di tutto (il founder l'aveva chiesto esplicitamente)
    che la chiave di un giorno è già il nome fisso (`"Lunedì"`...), mai un indice numerico, e che
    scambio pasti/modifica chat già preservano la data per costruzione (vedi "Giorno identificato
    da nome fisso..." in Pattern ricorrenti) — nessun bug da correggere lì. Cambiato solo il
    *formato* dell'etichetta in `etichettaGiorno` (`settimana.ts`): da "Lunedì 6 ottobre" per
    intero a "Gio 15 ott" abbreviato, su richiesta esplicita. Nuova schermata **Archivio**
    (`src/app/piano/[token]/archivio/`, sola lettura, nessuna tabella nuova — riusa le righe
    `weekly_plans` già esistenti) per aprire una settimana passata e vederne le date reali di
    allora; raggiungibile da un link in Profilo (Impostazioni), non in bottom nav (fissa a 6 tab).
    **Non implementato, deliberatamente, e annotato in `BACKLOG.md`**: uno stato dedicato "pasto
    fuori/vuoto" — oggi è solo un esempio di testo nella chat di modifica libera, nessuna
    marcatura speciale nello schema; costruirlo è lavoro separato (schema + validazione + UI), non
    implicito nella sola identificazione per data.
23. **(questa sessione)**: **Nutrizione: fonte CREA/INRAN vs stima AI**. Nuova tabella statica
    `nutrizione-crea.ts` (30 alimenti, valori ufficiali per 100g forniti dal founder) e
    `calcolaNutrizionePasto`, applicata da `applicaFonteNutrizione` in coda a `validaGiorni` (ogni
    punto che tocca il piano la eredita gratis, stesso principio di `assicuraVarieta`): un piatto
    usa il dato CREA SOLO se ogni ingrediente è coperto ed espresso in g/kg, altrimenti resta
    stima AI, mai mischiate. Match per parola intera con `esclusioni` mirate (vedi Pattern
    ricorrenti) per evitare falsi positivi (fagiolini/fagioli, mela/melanzana, latte vegetale/
    latte vaccino, ecc.). UI: etichetta "Fonte: CREA/INRAN - Centro di ricerca Alimenti e
    Nutrizione" o "Stima AI" accanto a ogni piatto in Menu, Archivio e Preferiti (non solo un
    disclaimer generale). Nuova colonna `preferiti.fonte_nutrizione` (migration
    `2026-10-13_add_fonte_nutrizione_preferiti.sql`, **da eseguire a mano sul Supabase di
    produzione**) per lo snapshot dei Preferiti. Nessuna modifica alla generazione del piano né
    alla validazione glutine.
24. **(questa sessione)**: **Pass di verifica su 28 comportamenti già costruiti** (non nuove
    feature) — testati uno a uno, due bug reali trovati e corretti:
    - **Onboarding**: solo lo step 0 (nome+restrizioni) bloccava "Avanti" se mancante; i passi
      "quante persone", "obiettivo" e "tempo per cucinare" si potevano saltare del tutto,
      salvando valori di fallback in silenzio. Aggiunta validazione anche lì (`onboarding/page.tsx`,
      `canAdvance`). Budget, supermercato e dati biometrici restano senza validazione, come
      dichiarato esplicitamente nel loro stesso testo in UI ("opzionale"/"indicativo"/"facoltativo").
    - **Pallino colorato urgenza Frigo**: era statico per categoria (`coloreScadenza`, rimossa) —
      un fresco mostrava sempre rosso dal giorno dell'acquisto alla scadenza, mai arancione/verde
      nel mentre. Sostituito con `coloreUrgenza(giorni)` in `scadenza-frigo.ts`, che usa i giorni
      REALI rimanenti (rosso ≤1, arancione ≤3, verde oltre o non tracciabile).
    Tutti gli altri 26 punti erano già corretti (verificati via codice + test automatici
    esistenti/nuovi, nessuna modifica necessaria). **Non verificabile dal vivo in questa sessione**:
    la chiave `ANTHROPIC_API_KEY` in uso ha credito esaurito ("credit balance too low") — nessuno
    smoke test reale è girato, quindi i comportamenti che dipendono dalla qualità di una
    generazione AI vera (obiettivi nutrizionali rispettati, batch cooking, scenari budget
    davvero diversi, memoria conversazionale multi-turno, OCR scontrino) sono confermati solo a
    livello di logica/prompt/test con risposte mockate, non con una chiamata reale. **Se la stessa
    chiave è usata in produzione, l'app probabilmente non riesce a generare né modificare piani
    in questo momento** — verificare il credito su console.anthropic.com prima di qualunque altra
    indagine se un utente segnala che "genera piano" non funziona.
25. **(questa sessione)**: **follow-up con chiave ricaricata** — i punti lasciati "non
    verificabili" sopra sono stati ritestati dal vivo (budget ridotto, ~4 euro), riusando gli
    stessi output tra più controlli invece di una generazione per singolo test (vedi riepilogo
    dato in chat per i dettagli numerici). Esito: **obiettivi nutrizionali per pasto** rispettati
    senza violare restrizioni, **batch cooking** confermato (ingredienti condivisi tra più piatti),
    **scenari budget** davvero diversi tra loro (overlap minimo), **memoria conversazionale** con
    riferimento implicito confermata su un test mirato pulito (stesso incremento applicato
    coerentemente a un giorno diverso), **OCR scontrino** (su uno scontrino finto ma plausibile,
    generato con Playwright, contenente articoli reali dalla lista della spesa) corretto al 100%:
    totale esatto, matching semantico anche con nomi abbreviati/diversi, articoli non presenti
    segnalati come tali, articoli extra isolati senza abbinamenti forzati. **Due comportamenti
    reali diversi dalle attese**, annotati in `BACKLOG.md` e lasciati così su richiesta esplicita
    del founder (non corretti): "giovedì mangio fuori" viene **rifiutato** da `modificaPiano`
    (`modificaApplicata: false`, nessun pasto rimosso) invece di sparire dal piano; "ho già
    comprato [ingrediente]" risponde `modificaApplicata: true` ma l'ingrediente resta comunque
    nella lista della spesa ricostruita. **Non verificabile neanche stavolta**: l'aggiornamento
    vero della calibrazione prezzi dopo un check-in (richiede scrivere/leggere `checkins` su
    Supabase, irraggiungibile da questo sandbox) e la persistenza degli scenari budget dopo un
    cambio tab nel browser reale (nessun fix del genere trovato nel codice).
26. **(questa sessione)**: **Validazione sicurezza generalizzata a 4 allergeni** (prima solo
    glutine). Verifica preliminare (come richiesto, prima di scrivere codice): la selezione
    restrizioni in onboarding era **già** un multi-select generico (`RESTRIZIONI_OPTIONS`) e
    includeva **già** "Frutta a guscio"/"Uova"/"Pesce/Crostacei" come opzioni — nulla da aggiungere
    in UI. La validazione a 4 livelli invece era scritta specificamente per il glutine (liste di
    parole chiave hardcoded, nessun parametro "quale allergene"); `regeneratePasto` era già
    completamente generico (mai la parola "glutine" nel suo prompt). Nessun campo ingrediente
    dedicato per gli allergeni, ma non serve: lo stesso meccanismo a parole chiave sul nome
    dell'ingrediente già usato per il glutine si estende naturalmente, nessun gap bloccante.
    **Rinominato** `glutine-check.ts` → `allergeni-check.ts`: nuovo tipo `Allergene`, una
    `RegoleAllergene` (4 liste di parole chiave) per ciascuno dei 4 allergeni,
    `allergeniDalProfilo()` mappa le restrizioni esatte del profilo agli allergeni gestiti,
    `categorizzaIngrediente`/`ingredientiNonAdatti`/`ingredientiDaVerificare`/
    `ingredientiDaSegnalare` ora accettano `Allergene[]` invece di operare implicitamente sul
    glutine — per un profilo con più restrizioni attive, un ingrediente rischioso per UNO
    qualsiasi degli allergeni richiesti blocca/segnala il pasto (unione per sicurezza, mai la
    media o il livello più permissivo). `piano-validazione.ts` aggiornato di conseguenza
    (`richiedeControlloGlutine` booleano → `allergeniAttivi: Allergene[]`), stessa identica
    struttura del ciclo di rigenerazione, nessun'altra modifica. Bug trovato e corretto durante i
    test: i qualificatori "vegano"/"vegetariano" non matchavano le forme declinate per genere
    ("maionese **vegana**" non conteneva la sottostringa esatta "vegano") — corretti in prefissi
    "vegan"/"vegetarian". Nessuna modifica alla generazione del piano o alla UI.

- Gli smoke test in `claude.smoke.test.ts` con un terzo argomento `30_000` **timeoutano spesso
  esattamente a 30s** in questo sandbox per latenza di rete verso l'API reale, non per un bug —
  osservato ripetutamente su più test diversi (generateMealPlan, modificaPiano), non è una
  regressione di una modifica specifica. Il timeout è hardcoded per test (il flag CLI
  `--testTimeout` NON lo sovrascrive): se lo rivedi fallire a ~30008ms, non è automaticamente un
  allarme, prova a isolarlo e a ripeterlo prima di sospettare una regressione reale.
- Ogni migration elencata sopra in `supabase/migrations/` va verificata con l'utente: se non è
  stata ancora eseguita sul Supabase di produzione, qualunque funzionalità che tocca quella
  tabella fallirà silenziosamente (azione via server action) o romperà la pagina (se letta
  durante il render SSR, come successo con `spesa_stato`).
- Non esiste ancora un controllo statico analogo al glutine per le altre restrizioni (lattosio,
  vegano, ecc.) — si affidano solo al prompt dell'AI, nessuna validazione post-generazione.

## Prima di chiudere questa sessione

Se stai per esaurire lo spazio di contesto: aggiorna questo file con quello che hai fatto in più
rispetto a quanto scritto sopra, poi commit + push (anche senza che l'utente lo chieda
esplicitamente per QUESTO file — è l'unico modo perché sopravviva al container effimero e serva
davvero da istruzioni per la prossima sessione).
