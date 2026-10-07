-- Groci: schema iniziale (profiles, weekly_plans, checkins)
-- Esegui questo script nel SQL Editor di Supabase (Project > SQL Editor > New query).
--
-- Modello di accesso attuale: nessun login, accesso tramite link_token univoco.
-- RLS è attiva su tutte le tabelle ma senza policy per anon/authenticated:
-- questo blocca l'accesso diretto dal browser. Tutte le operazioni devono
-- passare dal server Next.js usando la service role key (mai esposta al client).
-- Quando si passerà a Supabase Auth, si aggiungeranno policy basate su auth.uid().

create extension if not exists pgcrypto;

create table if not exists profiles (
  id uuid primary key default gen_random_uuid(),
  nome text not null,
  restrizioni text[] not null default '{}',
  household_size integer,
  obiettivo text,
  preferenze jsonb not null default '{}'::jsonb,
  tempo_max_cucina integer,
  budget_settimanale numeric(10, 2),
  supermercato text,
  modalita text not null default 'routine' check (modalita in ('routine', 'scoperta')),
  -- Dati biometrici opzionali, usati solo per il confronto nutrizionale LARN
  -- (vedi src/lib/larn.ts) — nessun piano viene generato in base a questi dati.
  sesso text check (sesso in ('M', 'F')),
  eta integer,
  peso_kg numeric(5, 2),
  altezza_cm numeric(5, 1),
  livello_attivita text check (livello_attivita in ('sedentario', 'moderato', 'attivo')),
  -- Obiettivi nutrizionali per singolo pasto, impostati esplicitamente
  -- dall'utente in Profilo (diversi dal confronto LARN sopra, che è
  -- derivato automaticamente dai dati biometrici): usati dal motore come
  -- vincolo aggiuntivo, stessa priorità dell'obiettivo generale, sempre
  -- sotto restrizioni alimentari e validazione di sicurezza (vedi
  -- buildContestoProfilo in src/lib/claude.ts). Tutti i campi opzionali.
  obiettivi_nutrizionali jsonb not null default '{}'::jsonb,
  link_token text not null unique default encode(gen_random_bytes(16), 'hex'),
  created_at timestamptz not null default now()
);

create table if not exists weekly_plans (
  id uuid primary key default gen_random_uuid(),
  profile_id uuid not null references profiles(id) on delete cascade,
  settimana date not null,
  meal_plan jsonb,
  grocery_list jsonb,
  -- Consumi dalla dispensa applicati da QUESTA versione del piano (oltre a
  -- grocery_list.rimasto) — serve a "annullare e rifare" correttamente
  -- l'effetto sulla dispensa quando il piano viene modificato più volte
  -- nella stessa settimana (vedi src/lib/dispensa.ts). Null se questa
  -- versione non ha applicato nulla alla dispensa (es. riuso in routine).
  consumi_dispensa jsonb,
  budget_stimato numeric(10, 2),
  modalita_usata text check (modalita_usata in ('routine', 'scoperta')),
  created_at timestamptz not null default now()
);

create index if not exists weekly_plans_profile_id_idx on weekly_plans(profile_id);

create table if not exists checkins (
  id uuid primary key default gen_random_uuid(),
  weekly_plan_id uuid not null references weekly_plans(id) on delete cascade,
  seguito_piano boolean,
  spreco boolean,
  categoria_spreco text,
  spesa_reale numeric(10, 2),
  retailer_usato text,
  created_at timestamptz not null default now()
);

create index if not exists checkins_weekly_plan_id_idx on checkins(weekly_plan_id);

-- Saldo corrente della "dispensa": quanto avanza di ogni ingrediente dopo
-- aver arrotondato alla confezione reale. Una riga per (profile_id,
-- ingrediente, unita) col saldo attuale; settimana = ultimo aggiornamento.
-- Scalata automaticamente dal fabbisogno dei piani futuri finché non si
-- esaurisce (vedi src/lib/dispensa.ts).
create table if not exists rimanenze (
  id uuid primary key default gen_random_uuid(),
  profile_id uuid not null references profiles(id) on delete cascade,
  ingrediente text not null,
  unita text not null,
  quantita numeric(10, 2) not null default 0,
  settimana date not null,
  created_at timestamptz not null default now(),
  unique (profile_id, ingrediente, unita)
);

create index if not exists rimanenze_profile_id_idx on rimanenze(profile_id);

-- Stato "acquistato" per ogni riga della lista della spesa di un piano,
-- separato dalla lista stessa (grocery_list) così da non doverla
-- rigenerare per salvare una spunta. Nessuna chiamata AI coinvolta.
create table if not exists spesa_stato (
  id uuid primary key default gen_random_uuid(),
  weekly_plan_id uuid not null references weekly_plans(id) on delete cascade,
  prodotto text not null,
  acquistato boolean not null default false,
  created_at timestamptz not null default now(),
  unique (weekly_plan_id, prodotto)
);

create index if not exists spesa_stato_weekly_plan_id_idx on spesa_stato(weekly_plan_id);

-- Feedback rapido (pollice su/giù + commento facoltativo), mostrato di
-- rado dopo un'azione chiave o a fine check-in. Dato a uso interno, mai
-- mostrato come punteggio all'utente.
create table if not exists feedback_rapido (
  id uuid primary key default gen_random_uuid(),
  profile_id uuid not null references profiles(id) on delete cascade,
  contesto text not null,
  risposta boolean not null,
  commento text,
  created_at timestamptz not null default now()
);

create index if not exists feedback_rapido_profile_id_idx on feedback_rapido(profile_id);

-- Preferiti: icona cuore su un piatto nel Menu salva un'istantanea del
-- piatto (nome, ingredienti, nutrizione, preparazione) così resta
-- consultabile anche quando il piano che lo conteneva viene sostituito da
-- uno nuovo. Versione semplice: nessuna influenza sul motore di
-- generazione, solo una lista consultabile.
create table if not exists preferiti (
  id uuid primary key default gen_random_uuid(),
  profile_id uuid not null references profiles(id) on delete cascade,
  nome text not null,
  tipo text not null check (tipo in ('pranzo', 'cena')),
  ingredienti jsonb not null,
  tempo_preparazione_min integer,
  nutrizione jsonb,
  preparazione jsonb,
  created_at timestamptz not null default now(),
  unique (profile_id, nome)
);

create index if not exists preferiti_profile_id_idx on preferiti(profile_id);

alter table profiles enable row level security;
alter table weekly_plans enable row level security;
alter table checkins enable row level security;
alter table rimanenze enable row level security;
alter table spesa_stato enable row level security;
alter table feedback_rapido enable row level security;
alter table preferiti enable row level security;
