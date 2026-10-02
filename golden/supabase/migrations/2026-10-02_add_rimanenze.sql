-- Aggiunge la tabella 'rimanenze' (dispensa persistente tra settimane).
-- Esegui nel SQL Editor di Supabase sul database esistente.

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

alter table rimanenze enable row level security;
