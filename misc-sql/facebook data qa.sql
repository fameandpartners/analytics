SELECT count(DISTINCT user_id)
FROM wedding_atelier_users_event_roles uer
INNER JOIN facebook_data fb ON uer.user_id = fb.spree_user_id