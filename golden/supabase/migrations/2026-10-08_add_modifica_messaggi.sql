-- Memoria conversazionale per le modifiche in linguaggio naturale (vedi
-- testoCronologia in src/lib/claude.ts e /api/piano/modifica): ogni
-- richiesta di modifica su un piano, con l'esito, così le richieste
-- successive sulla stessa conversazione possono capire riferimenti
-- impliciti ("anche lì", "idem per cena"). Include anche il costo stimato
-- della chiamata (token + USD), per monitorarlo nel tempo.
-- Esegui nel SQL Editor di Supabase sul database esistente.

create table if not exists modifica_messaggi (
  id uuid primary key default gen_random_uuid(),
  weekly_plan_id uuid not null references weekly_plans(id) on delete cascade,
  messaggio text not null,
  modifica_applicata boolean not null,
  motivo_rifiuto text,
  input_tokens integer,
  output_tokens integer,
  costo_stimato_usd numeric(10, 6),
  created_at timestamptz not null default now()
);

create index if not exists modifica_messaggi_weekly_plan_id_idx on modifica_messaggi(weekly_plan_id);

alter table modifica_messaggi enable row level security;
