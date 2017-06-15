SELECT
    fb1.ad_insight_id,
    SUM(CASE WHEN action_type = 'offsite_conversion.fb_pixel_purchase' 
            THEN action_value ELSE 0 END) purchases,
    SUM(CASE WHEN action_type = 'offsite_conversion.fb_pixel_add_to_cart'
            THEN action_value ELSE 0 END) adds_to_cart
FROM 
    (SELECT 
        id ad_insight_id, 
        facebook_ad_id,
        json_array_elements(actions) ->> 'action_type' action_type, 
        (json_array_elements(actions) ->> 'value')::int action_value 
    FROM facebook_ad_insights) fb1 
WHERE fb1.action_type IN (
    'offsite_conversion.fb_pixel_purchase',
    'offsite_conversion.fb_pixel_add_to_cart'
)
GROUP BY fb1.ad_insight_id;

SELECT 
    date_start::DATE ad_date, 
    a.name ad_name,
    sum(reach) reach, 
    sum(replace(social_impressions::TEXT, '"', '')::INT) impressions,
    sum(spend) amount_spent_aud,
    sum(clicks) clicks
FROM facebook_ad_insights i 
INNER JOIN facebook_ads a
    ON a.id = i.facebook_ad_id
INNER JOIN (
    SELECT
        fb1.ad_insight_id,
        SUM(CASE WHEN action_type = 'offsite_conversion.fb_pixel_purchase' 
                THEN action_value ELSE 0 END) purchases,
        SUM(CASE WHEN action_type = 'offsite_conversion.fb_pixel_add_to_cart'
                THEN action_value ELSE 0 END) adds_to_cart
    FROM 
        (SELECT 
            id ad_insight_id, 
            facebook_ad_id,
            json_array_elements(actions) ->> 'action_type' action_type, 
            (json_array_elements(actions) ->> 'value')::int action_value 
        FROM facebook_ad_insights) fb1 
    WHERE fb1.action_type IN (
        'offsite_conversion.fb_pixel_purchase',
        'offsite_conversion.fb_pixel_add_to_cart'
    )
    GROUP BY fb1.ad_insight_id
)
WHERE a.name like '%\_%'
GROUP BY ad_date, ad_name 
ORDER BY ad_date, ad_name;

select targeting -> 'publisher_platforms' platform from facebook_adsets;