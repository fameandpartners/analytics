SELECT COUNT(DISTINCT spree_user_id)
FROM wedding_atelier_user_profiles p 
INNER JOIN wedding_atelier_users_event_roles r
	ON p.spree_user_id = r.user_id
INNER JOIN wedding_atelier_event_roles er
	ON er.id = r.event_role_id
INNER JOIN wedding_atelier_events e
	ON e.id = er.resource_id