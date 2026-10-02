-- Golden: schema iniziale (profiles, weekly_plans, checkins)
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
  link_token text not null unique default encode(gen_random_bytes(16), 'hex'),
  created_at timestamptz not null default now()
);

create table if not exists weekly_plans (
  id uuid primary key default gen_random_uuid(),
  profile_id uuid not null references profiles(id) on delete cascade,
  settimana date not null,
  meal_plan jsonb,
  grocery_list jsonb,
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

alter table profiles enable row level security;
alter table weekly_plans enable row level security;
alter table checkins enable row level security;
alter table rimanenze enable row level security;
