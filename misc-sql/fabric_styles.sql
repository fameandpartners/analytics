SELECT 
	v.sku style_number,
	p.name product_name,
	pp.value fabric_description
FROM spree_variants v 
INNER JOIN spree_products p 
	ON v.product_id = p.id
LEFT JOIN spree_product_properties pp 
	ON pp.product_id = p.id
WHERE v.is_master
	AND NOT p.hidden
	AND (p.deleted_at IS NULL OR p.deleted_at > current_date)
	AND pp.property_id = 6
	AND pp.value IS NOT NULL

-- SELECT  v.sku style_number, p.name product_name, pp.value fabric_description FROM spree_variants v  INNER JOIN spree_products p  ON v.product_id = p.id LEFT JOIN spree_product_properties pp  ON pp.product_id = p.id WHERE v.is_master AND NOT p.hidden AND (p.deleted_at IS NULL OR p.deleted_at > current_date) AND pp.property_id = 6 AND pp.value IS NOT NULL
