SELECT p.name product_name,
       p.available_on::DATE available_on,
       sku.style_number,
       sku.color
FROM spree_products p
INNER JOIN
  ( SELECT DISTINCT product_id,
                    style_number,
                    color_name color
   FROM global_skus
   WHERE color_name IS NOT NULL) sku ON sku.product_id = p.id
WHERE NOT p.hidden
  AND (p.deleted_at IS NULL
       OR p.deleted_at >= CURRENT_DATE)
  AND (p.available_on <= CURRENT_DATE)
ORDER BY product_name -- SELECT p.name product_name, p.available_on::DATE available_on, sku.style_number, sku.color FROM spree_products p INNER JOIN (SELECT DISTINCT product_id, style_number, color_name color FROM global_skus WHERE color_name IS NOT NULL ) sku ON sku.product_id = p.id WHERE NOT p.hidden AND (p.deleted_at IS NULL OR p.deleted_at >= CURRENT_DATE) AND (p.available_on <= CURRENT_DATE) ORDER BY product_name
-- to 'products live on site as of 2017-03-30.csv' with csv header
