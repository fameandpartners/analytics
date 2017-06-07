select 
	count(distinct user_id) 
from wedding_atelier_users_event_roles uer 
inner join facebook_data fb 
	on uer.user_id = fb.spree_user_id