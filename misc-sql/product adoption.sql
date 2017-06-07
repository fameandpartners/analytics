SELECT 
	fs.first_sale_date -  p.created_at::DATE days_to_first_sale,
	fs.items_sold items_sold
FROM spree_products p
INNER JOIN 
	(SELECT 
		v.product_id,
		MIN(o.completed_at)::DATE first_sale_date,
		COUNT(*) items_sold
	FROM spree_line_items li
	INNER JOIN spree_orders o 
		ON o.id = li.order_id
	INNER JOIN spree_variants v
		ON v.id = li.variant_id
	WHERE o.completed_at IS NOT NULL
	GROUP BY 
		v.product_id) fs
	ON fs.product_id = p.id
WHERE fs.items_sold > 100