SELECT 
	completed_at::DATE order_date, 
	p.name style_name,
	COUNT(*) units
FROM spree_line_items li
INNER JOIN spree_orders o
	ON o.id = li.order_id
INNER JOIN spree_variants v
	ON v.id = li.variant_id
INNER JOIN spree_products p
	On p.id = v.product_id
WHERE p.id IN (1313,1301,1304,1300,1315,1314,1303,1297,1302,1298,1296,1299,1295)
	AND o.completed_at IS NOT NULL
	AND o.state = 'complete'
	AND o.payment_state = 'paid'
GROUP BY 
	completed_at::DATE,
	p.name

-- SELECT  completed_at::DATE order_date,  p.name style_name, COUNT(*) units FROM spree_line_items li INNER JOIN spree_orders o ON o.id = li.order_id INNER JOIN spree_variants v ON v.id = li.variant_id INNER JOIN spree_products p On p.id = v.product_id WHERE p.id IN (1313,1301,1304,1300,1315,1314,1303,1297,1302,1298,1296,1299,1295) AND o.completed_at IS NOT NULL AND o.state = 'complete' AND o.payment_state = 'paid' GROUP BY  completed_at::DATE, p.name
