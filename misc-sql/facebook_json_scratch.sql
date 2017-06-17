SELECT 
    date_start::DATE ad_date, 
    ads.name ad_name,
    copy.image_array ad_images,
    (adsets.targeting -> 'publisher_platforms')::TEXT platforms,
    insights.reach reach, 
    replace(insights.social_impressions::TEXT, '"', '')::INT impressions,
    insights.spend amount_spent_aud,
    insights.clicks clicks,
    conversions.purchases purchases,
    conversions.adds_to_cart adds_to_cart
FROM facebook_ad_insights insights 
INNER JOIN facebook_ads ads
    ON ads.id = insights.facebook_ad_id
INNER JOIN (
    SELECT
        ad_insight_id,
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
        FROM facebook_ad_insights) ad_metrics 
    WHERE action_type IN (
        'offsite_conversion.fb_pixel_purchase',
        'offsite_conversion.fb_pixel_add_to_cart'
    )
    GROUP BY ad_insight_id
) conversions ON conversions.ad_insight_id = insights.id
INNER JOIN facebook_adsets adsets
    ON adsets.id = ads.facebook_adset_id::INT
INNER JOIN (
    SELECT facebook_ad_id, STRING_AGG('<img src=' || image_url || '>', '') image_array, COUNT(*)
    FROM facebook_ad_creatives
    GROUP BY facebook_ad_id
    ORDER BY COUNT(*) DESC
) copy ON copy.facebook_ad_id = ads.id 
WHERE ads.name like '%\_%';
