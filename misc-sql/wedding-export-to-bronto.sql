SELECT u.email,
       w.date wedding_date,
       w.name wedding_name,
       COALESCE(ed.dresses, 0) dresses,
       w.created_at::DATE board_create_date,
       ed.first_dress_add_date,
       ed.last_dress_add_date,
       COALESCE(i.accepted_invitations, 0) accepted_invitations
FROM wedding_atelier_events w
INNER JOIN spree_users u ON w.owner_id = u.id
LEFT JOIN
  ( SELECT event_id,
           COUNT(*) dresses,
           MIN(created_at::DATE) first_dress_add_date,
           MAX(created_at::DATE) last_dress_add_date
   FROM wedding_atelier_event_dresses
   GROUP BY event_id) ed ON ed.event_id = w.id
LEFT JOIN
  ( SELECT event_id,
           COUNT(*) accepted_invitations
   FROM wedding_atelier_invitations
   WHERE STATE = 'accepted'
   GROUP BY event_id) i ON w.id = i.event_id --SELECT  u.email,  w.date wedding_date,  w.name wedding_name,  COALESCE(ed.dresses, 0) dresses, w.created_at::DATE board_create_date, ed.first_dress_add_date, ed.last_dress_add_date, COALESCE(i.accepted_invitations, 0) accepted_invitations FROM wedding_atelier_events w INNER JOIN spree_users u ON w.owner_id = u.id LEFT JOIN ( SELECT  event_id,  COUNT(*) dresses,  MIN(created_at::DATE) first_dress_add_date, MAX(created_at::DATE) last_dress_add_date FROM wedding_atelier_event_dresses GROUP BY event_id) ed ON ed.event_id = w.id LEFT JOIN ( SELECT event_id, COUNT(*) accepted_invitations FROM wedding_atelier_invitations WHERE state = 'accepted' GROUP BY event_id) i  ON w.id = i.event_id
