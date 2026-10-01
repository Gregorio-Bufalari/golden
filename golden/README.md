# Golden

Next.js (App Router) + TypeScript + Tailwind CSS, pronto per il deploy su Vercel, con integrazione Supabase già predisposta.

## Setup locale

```bash
npm install
cp .env.example .env.local   # poi compila le chiavi Supabase
npm run dev
```

Apri [http://localhost:3000](http://localhost:3000).

## Collegare un progetto Supabase (free tier)

1. Vai su [supabase.com](https://supabase.com) e crea un account gratuito (o accedi con GitHub).
2. Clicca **New project**, scegli un'organizzazione, dai un nome al progetto (es. `golden`), imposta una password per il database e seleziona una region vicina.
3. Attendi il provisioning del progetto (1-2 minuti).
4. Vai su **Project Settings > API**: copia `Project URL` e la chiave `anon public`.
5. Nel progetto Next.js, crea `.env.local` a partire da `.env.example` e incolla i valori:

   ```bash
   NEXT_PUBLIC_SUPABASE_URL=https://xxxxxxxxxxxx.supabase.co
   NEXT_PUBLIC_SUPABASE_ANON_KEY=your-anon-key
   ```

6. Riavvia `npm run dev`: la home page mostra lo stato della connessione a Supabase.

Il client Supabase è già configurato in `src/lib/supabase/`:
- `client.ts` — client per i Client Component (browser).
- `server.ts` — client per Server Component / Route Handler (cookie-aware).
- `middleware.ts` — refresh della sessione, usato da `src/proxy.ts` (in Next.js 16 il Middleware si chiama Proxy).

## Deploy su Vercel

1. Pusha il repo su GitHub (già fatto se stai leggendo questo da lì).
2. Su [vercel.com/new](https://vercel.com/new) importa il repository.
3. In **Environment Variables** aggiungi `NEXT_PUBLIC_SUPABASE_URL` e `NEXT_PUBLIC_SUPABASE_ANON_KEY` con gli stessi valori di `.env.local`.
4. Deploy. Vercel rileva automaticamente Next.js e configura build/output.

## Script disponibili

```bash
npm run dev     # sviluppo
npm run build   # build di produzione
npm run start   # avvia la build di produzione
npm run lint    # eslint
```
