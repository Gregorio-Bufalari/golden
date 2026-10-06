-- Feedback rapido (pollice su/giù + commento facoltativo), mostrato di
-- rado dopo un'azione chiave o a fine check-in. Dato a uso interno, mai
-- mostrato come punteggio all'utente.
-- Esegui nel SQL Editor di Supabase sul database esistente.

create table if not exists feedback_rapido (
  id uuid primary key default gen_random_uuid(),
  profile_id uuid not null references profiles(id) on delete cascade,
  contesto text not null,
  risposta boolean not null,
  commento text,
  created_at timestamptz not null default now()
);

create index if not exists feedback_rapido_profile_id_idx on feedback_rapido(profile_id);

alter table feedback_rapido enable row level security;
