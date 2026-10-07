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
                                adattaBudget, regeneratePasto); system prompt condivisi
src/lib/piano-validazione.ts — pipeline di validazione POST-generazione, nessuna chiamata AI
                                fuori da qui per la sicurezza: validaGiorni (controllo glutine
                                a 4 livelli), assicuraVarieta (niente piatti duplicati nella
                                settimana), adattaEntroBudget (retry budget)
src/lib/glutine-check.ts     — classificazione statica per ingrediente a 4 categorie (vedi sotto)
src/lib/grocery.ts           — costruzione lista della spesa da un piano validato
src/lib/dispensa.ts          — saldo "rimanenze" tra una settimana e l'altra
src/lib/conservazione.ts     — categoria di conservazione per ingrediente (Frigo)
src/lib/sprechi-evitati.ts   — stima € "sprechi evitati" (Andamento)
src/lib/varieta-giorno.ts    — controllo leggero post-scambio pasti (stesso ingrediente
                                "principale" a pranzo e cena dello stesso giorno?)
src/lib/settimana.ts         — data reale di un giorno della settimana, solo per display
src/lib/quantita.ts          — formattazione quantità per porzione, scalata su household_size
src/lib/scadenza-frigo.ts    — data di scadenza stimata per voce del Frigo (euristica per
                                categoria ingrediente) + giorni mancanti, per il banner
src/lib/calibrazione-prezzi.ts — "learning loop" prezzi: fattore correttivo derivato dai
                                check-in passati (spesa_reale vs budget_stimato) PER
                                SUPERMERCATO, applicato sopra la fascia statica in grocery.ts

src/app/api/piano/generate/route.ts  — genera un piano nuovo (o riusa l'ultimo in modalità
                                        "routine"), orchestration completa
src/app/api/piano/modifica/route.ts  — UNICO endpoint per ogni modifica in linguaggio naturale
                                        al piano: "Proponi un piatto diverso", +/- nutrienti,
                                        "Sostituisci"/"Non l'ho trovato", ecc. Passa sempre da
                                        validaGiorni — nessuna validazione duplicata altrove.

src/app/piano/[token]/
  layout.tsx, bottom-nav.tsx, page-header.tsx  — shell condivisa (6 tab: Menu, Spesa, Frigo,
                                                   Andamento, Check-in, Preferiti)
  menu/                — schermata principale: piano settimanale, scambio pasti, preferiti,
                          confronto nutrizionale LARN, box "Modifica il piano" (NL)
  spesa/                — lista della spesa, spunte, "Non l'ho trovato"
  frigo/                — dispensa residua con pallino colorato per urgenza di consumo
  andamento/            — KPI: risparmio vs budget, % settimane senza sprechi, sprechi evitati €
  checkin-form.tsx      — check-in settimanale (seguito il piano? sprecato? spesa reale?)
  impostazioni/         — Profilo: riepilogo in sola lettura + pulsante Modifica in fondo
  preferiti/            — lista consultabile dei piatti salvati come preferiti
  actions.ts            — server action scambiaPasti() + setModalita()

src/components/supermercato-selector.tsx — componente condiviso di scelta supermercato,
  riusato in Onboarding, Profilo (Impostazioni) e Check-in: stessa lista
  (SUPERMERCATO_OPTIONS) e stesso stile pillola ovunque, nessuna duplicazione.
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
- **Nessuna influenza sul motore AI senza che sia esplicitamente richiesto** — es. "Preferiti" è
  stato costruito deliberatamente come sola lettura, nessun collegamento ai prompt di
  generazione ("versione semplice" richiesta esplicitamente).
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

## Validazione sicurezza glutine — a 4 livelli (cambiata di recente)

`src/lib/glutine-check.ts`: `categorizzaIngrediente(nome)` restituisce una di:
- `"verificato"` — certificazione esplicita ("senza glutine", "certificato")
- `"informazioni_sufficienti"` — nessun segnale di rischio, o sicuro per natura (es. "pasta di riso")
- `"da_verificare"` — dipende da marca/formulazione (dado vegetale, salsa di soia, besciamella,
  avena, cuscus, malto) — **non blocca** il pasto, solo segnalato per controllo etichetta
- `"non_adatto"` — glutine senza ambiguità (farina di frumento, pane, pasta, orzo, segale, farro,
  kamut, seitan) — **blocca** il pasto, fa scattare un tentativo di rigenerazione AI
  (`regeneratePasto`, max 2 tentativi)

Il banner nel Menu ("Verifica necessaria" di una volta) ora distingue rosso/clay ("Non adatto",
rigenerazione fallita) da ambra/honey ("Da verificare", solo da controllare in etichetta).

## Ordine cronologico di cosa è stato costruito (PR #16 → #49, tutte mergiate)

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
15. **(questa sessione)**: **Obiettivi nutrizionali per pasto** in Profilo (nuova sezione:
    calorie min/max, proteine minime, carboidrati/grassi massimi per pasto, tutti opzionali) —
    diversi dal confronto LARN (automatico, dai dati biometrici, solo informativo): questi sono
    un target esplicito passato al motore come vincolo aggiuntivo, **stessa priorità
    dell'obiettivo generale**, sempre sotto restrizioni alimentari e budget (vedi
    `obiettiviNutrizionaliTesto` in `claude.ts`). Non aggiunto in Onboarding (richiesto solo per
    Profilo), nessuna validazione post-generazione (a differenza del glutine): è un'istruzione nel
    prompt, non un vincolo rigido verificato dopo

## Cose da sapere / residuo noto

- Lo smoke test `"generateMealPlan produce un piano di 7 giorni senza ingredienti a rischio
  glutine"` in `claude.smoke.test.ts` **timeouta spesso a 30s** in questo sandbox per latenza di
  rete verso l'API reale, non per un bug — osservato ripetutamente, non è una regressione.
  Se lo rivedi fallire, non è automaticamente un allarme.
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
