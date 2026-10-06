-- Preferiti: icona cuore su un piatto nel Menu salva un'istantanea del
-- piatto (nome, ingredienti, nutrizione, preparazione) così resta
-- consultabile anche quando il piano che lo conteneva viene sostituito da
-- uno nuovo. Versione semplice: nessuna influenza sul motore di
-- generazione, solo una lista consultabile.
-- Esegui nel SQL Editor di Supabase sul database esistente.

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

alter table preferiti enable row level security;
