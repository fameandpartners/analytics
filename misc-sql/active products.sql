SELECT *
FROM spree_products p
WHERE NOT p.hidden
  AND (p.deleted_at IS NULL
       OR p.deleted_at >= CURRENT_DATE)
  AND (p.available_on <= CURRENT_DATE)