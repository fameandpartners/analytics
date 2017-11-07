SELECT DISTINCT p.name product_name,
                p.available_on::DATE available_on,
                g.style_number
FROM spree_products p
INNER JOIN global_skus g ON g.product_id = p.id
WHERE NOT p.hidden
  AND (p.deleted_at IS NULL
       OR p.deleted_at >= CURRENT_DATE)
  AND (p.available_on <= CURRENT_DATE) -- SELECT DISTINCT p.name product_name, p.available_on::DATE available_on, g.style_number FROM spree_products p INNER JOIN global_skus g ON g.product_id = p.id WHERE NOT p.hidden AND (p.deleted_at IS NULL OR p.deleted_at >= CURRENT_DATE) AND (p.available_on <= CURRENT_DATE)