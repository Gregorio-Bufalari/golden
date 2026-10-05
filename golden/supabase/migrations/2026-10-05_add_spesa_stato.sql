-- Stato "acquistato" per ogni riga della lista della spesa di un piano,
-- separato dalla lista stessa (grocery_list) così da non doverla
-- rigenerare per salvare una spunta. Una riga per (weekly_plan_id,
-- prodotto); nessuna chiamata AI coinvolta.
-- Esegui nel SQL Editor di Supabase sul database esistente.

create table if not exists spesa_stato (
  id uuid primary key default gen_random_uuid(),
  weekly_plan_id uuid not null references weekly_plans(id) on delete cascade,
  prodotto text not null,
  acquistato boolean not null default false,
  created_at timestamptz not null default now(),
  unique (weekly_plan_id, prodotto)
);

create index if not exists spesa_stato_weekly_plan_id_idx on spesa_stato(weekly_plan_id);

alter table spesa_stato enable row level security;
