SELECT date_start::DATE ad_date,
       ads.name ad_name,
       sum(insights.reach) reach,
       sum(replace(insights.social_impressions::TEXT, '"', '')::INT) impressions,
       sum(insights.spend) amount_spent_aud,
       sum(insights.clicks) clicks,
       sum(conversions.purchases) purchases,
       sum(conversions.adds_to_cart) adds_to_cart
FROM facebook_ad_insights insights
INNER JOIN facebook_ads ads ON ads.id = insights.facebook_ad_id
INNER JOIN
  ( SELECT fb1.ad_insight_id,
           SUM(CASE WHEN action_type = 'offsite_conversion.fb_pixel_purchase' THEN action_value ELSE 0 END) purchases,
           SUM(CASE WHEN action_type = 'offsite_conversion.fb_pixel_add_to_cart' THEN action_value ELSE 0 END) adds_to_cart
   FROM
     (SELECT id ad_insight_id,
             facebook_ad_id,
             json_array_elements(actions) ->> 'action_type' action_type,
                                                            (json_array_elements(actions) ->> 'value')::int action_value
      FROM facebook_ad_insights) fb1
   WHERE fb1.action_type IN ( 'offsite_conversion.fb_pixel_purchase',
                              'offsite_conversion.fb_pixel_add_to_cart' )
   GROUP BY fb1.ad_insight_id) conversions ON conversions.ad_insight_id = insights.id
WHERE ads.name LIKE '%\_%'
GROUP BY ad_date,
         ad_name
ORDER BY ad_date,
         ad_name;


SELECT targeting -> 'publisher_platforms' platform
FROM facebook_adsets;