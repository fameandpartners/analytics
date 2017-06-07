-- monthly rates
SELECT
	EXTRACT(YEAR FROM completed_at) order_year,
	EXTRACT(MONTH FROM completed_at) order_month,
	SUM(COALESCE(c.custom_color, 0) * li.price * li.quantity)
	/ 
	SUM(li.price * li.quantity) color_customization_rate
FROM spree_line_items li
INNER JOIN spree_orders o
	ON o.id = li.order_id
LEFT JOIN
	(SELECT 
		lip.line_item_id, 
		MAX(CASE WHEN pcv.custom THEN 1 ELSE 0 END) custom_color
	FROM line_item_personalizations lip 
	LEFT JOIN product_color_values pcv 
		ON lip.color_id = pcv.id 
	GROUP BY line_item_id) c
	ON c.line_item_id = li.id
WHERE o.completed_at IS NOT NULL
	AND o.state = 'complete'
	AND o.payment_state = 'paid'
	AND o.shipment_state = 'shipped'
	AND o.currency = 'AUD'
	AND o.completed_at >= '2015-01-01'
GROUP BY 
	EXTRACT(YEAR FROM completed_at),
	EXTRACT(MONTH FROM completed_at)
ORDER BY 
	EXTRACT(YEAR FROM completed_at),
	EXTRACT(MONTH FROM completed_at);

-- SELECT EXTRACT(YEAR FROM completed_at) order_year, EXTRACT(MONTH FROM completed_at) order_month, SUM(COALESCE(c.custom_color, 0) * li.price * li.quantity) /  SUM(li.price * li.quantity) color_customization_rate FROM spree_line_items li INNER JOIN spree_orders o ON o.id = li.order_id LEFT JOIN (SELECT  lip.line_item_id,  MAX(CASE WHEN pcv.custom THEN 1 ELSE 0 END) custom_color FROM line_item_personalizations lip  LEFT JOIN product_color_values pcv  ON lip.color_id = pcv.id  GROUP BY line_item_id) c ON c.line_item_id = li.id WHERE o.completed_at IS NOT NULL AND o.state = 'complete' AND o.payment_state = 'paid' AND o.shipment_state = 'shipped' AND o.currency = 'AUD' AND o.completed_at >= '2015-01-01' GROUP BY  EXTRACT(YEAR FROM completed_at), EXTRACT(MONTH FROM completed_at) ORDER BY  EXTRACT(YEAR FROM completed_at), EXTRACT(MONTH FROM completed_at)

-- line item level for audit
SELECT 
	li.id line_item_id, 
	v.product_id,
	o.number order_number, 
	o.completed_at::DATE order_date,
	COALESCE(c.custom_color, 0) custom_color, 
	li.price, 
	li.quantity,
	p.id IS NOT NULL active_product,
	o.total order_total
FROM spree_line_items li
INNER JOIN spree_orders o
	ON o.id = li.order_id
LEFT JOIN spree_variants v 
	ON li.variant_id = v.id
LEFT JOIN (
	SELECT 
		lip.line_item_id, 
		MAX(CASE WHEN pcv.custom THEN 1 ELSE 0 END) custom_color
	FROM line_item_personalizations lip 
	LEFT JOIN product_color_values pcv 
		ON lip.color_id = pcv.id 
	GROUP BY line_item_id) c
	ON c.line_item_id = li.id
LEFT JOIN (
	SELECT id
	FROM spree_products
	WHERE NOT hidden
		AND (deleted_at IS NULL OR deleted_at > CURRENT_DATE)
		AND available_on <= CURRENT_DATE) p
	ON p.id = v.product_id
WHERE o.completed_at IS NOT NULL
	AND o.state = 'complete'
	AND o.payment_state = 'paid'
	AND o.shipment_state = 'shipped'
	AND o.completed_at >= '2015-01-01'
ORDER BY order_date, order_number

-- products where all colors are custom
select product_id, count(*) colors, sum(custom::int) custom_colors
from product_color_values
group by product_id
HAVING count(*) = sum(custom::int)

-- best method for calculating color customization rate must be done in R
-- sudo code calc
-- customize an item is $16 or 16 AUD every time so use that rule to figure out what dresses have been customized
-- color_customized = spree_orders.total - (SUM(spree_line_items.price * spree_line_items.quantity) 
-- 										+ SUM(spree_adjustments.amount)
-- 										+ SUM(customization_values.price)) % 16