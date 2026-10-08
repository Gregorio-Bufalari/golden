-- Notifiche push vere (browser/PWA) per le scadenze del Frigo, al posto
-- del solo banner in-app — vedi src/lib/notifiche-scadenza.ts e
-- /api/push/notifica-scadenze. Una riga per dispositivo/browser
-- sottoscritto: un profilo può averne più di una.
-- Esegui nel SQL Editor di Supabase sul database esistente.

create table if not exists push_subscriptions (
  id uuid primary key default gen_random_uuid(),
  profile_id uuid not null references profiles(id) on delete cascade,
  endpoint text not null unique,
  p256dh text not null,
  auth_key text not null,
  created_at timestamptz not null default now()
);

create index if not exists push_subscriptions_profile_id_idx on push_subscriptions(profile_id);

alter table push_subscriptions enable row level security;
