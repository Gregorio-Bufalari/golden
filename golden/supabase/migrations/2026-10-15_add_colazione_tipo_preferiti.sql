-- Il piano settimanale ora include anche la colazione (non solo
-- pranzo+cena): i Preferiti devono poter salvare un'istantanea anche di un
-- pasto di tipo "colazione".
-- Esegui nel SQL Editor di Supabase sul database esistente.

alter table preferiti drop constraint if exists preferiti_tipo_check;
alter table preferiti add constraint preferiti_tipo_check check (tipo in ('colazione', 'pranzo', 'cena'));
