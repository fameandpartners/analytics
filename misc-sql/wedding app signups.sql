-- total sign ups

SELECT COUNT(DISTINCT email)
FROM spree_users
WHERE wedding_atelier_signup_step = 'completed';

 -- total sign ups method 2

SELECT COUNT(DISTINCT u.email)
FROM wedding_atelier_events w
INNER JOIN spree_users u ON w.owner_id = u.id;

 -- sign ups per day

SELECT w.signup_date,
       COUNT(DISTINCT u.email)
FROM spree_users u
INNER JOIN
  ( SELECT u.email,
           MIN(w.created_at::DATE) signup_date
   FROM wedding_atelier_events w
   INNER JOIN spree_users u ON w.owner_id = u.id
   GROUP BY u.email) w ON w.email = u.email
GROUP BY w.signup_date;

