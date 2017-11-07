SELECT DISTINCT ads.name ad_name, copy.image_url ad_image
FROM facebook_ads ads
INNER JOIN facebook_ad_creatives copy ON copy.facebook_ad_id = ads.id
