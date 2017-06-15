select * from 
	(select 
		id ad_insight_id, 
		facebook_ad_id,
		json_array_elements(actions) ->> 'action_type' action_type, 
		(json_array_elements(actions) ->> 'value')::int action_value 
	from facebook_ad_insights) fb1 
where fb1.action_type IN (
	'offsite_conversion.fb_pixel_purchase',
	'offsite_conversion.fb_pixel_add_to_cart',
	'link_click'
);

select 
	date_start::DATE ad_date, 
	sum(reach) reach, 
	sum(clicks) clicks, 
	sum(clicks) / sum(reach) ctr 
from facebook_ad_insights 
group by ad_date order by ad_date;