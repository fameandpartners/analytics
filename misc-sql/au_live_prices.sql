SELECT DISTINCT p.id pid,
                p.name product_name,
                price.amount,
                price.currency
FROM spree_products p
INNER JOIN spree_variants v ON v.product_id = p.id
INNER JOIN spree_prices price ON price.variant_id = v.id
WHERE NOT p.hidden
  AND (p.deleted_at IS NULL
       OR p.deleted_at >= CURRENT_DATE)
  AND (p.available_on <= CURRENT_DATE)
  AND price.currency = 'AUD' -- SELECT DISTINCT p.id pid, p.name product_name, price.amount, price.currency FROM spree_products p  INNER JOIN spree_variants v  ON v.product_id = p.id INNER JOIN spree_prices price ON price.variant_id = v.id WHERE NOT p.hidden AND (p.deleted_at IS NULL OR p.deleted_at >= CURRENT_DATE) AND (p.available_on <= CURRENT_DATE) AND price.currency = 'AUD'