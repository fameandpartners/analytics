SELECT 
	v.sku style_number,
	c.name fabric_name,
	p.created_at style_created
FROM spree_variants v
INNER JOIN spree_products p 
	ON p.id = v.product_id
LEFT JOIN fabric_cards c
	ON c.id = p.fabric_card_id
WHERE v.is_master
	AND c.name IS NOT NULL