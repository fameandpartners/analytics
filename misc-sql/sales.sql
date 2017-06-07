WITH sales AS 
(SELECT 
	li.id, 
	li.order_id, 
	o.number order_number, 
	li.quantity, 
	li.price, 
	li.currency, 
	INITCAP(sa.city) ship_city, 
	INITCAP(ss.name) ship_state, 
	INITCAP(sc.name) ship_country, 
	o.completed_at::date order_date, 
	CASE WHEN s.ship_date IS NULL 
		THEN o.projected_delivery_date::DATE ELSE s.ship_date::DATE END ship_date, 
	o.email, 
	o.user_id, 
	INITCAP(o.user_first_name) || ' ' || INITCAP(o.user_last_name) customer_name, 
	p.id product_id, INITCAP(p.name) style_name, 
	ir.refund_amount IS NOT NULL item_returned, 
	ir.refund_amount refund_amount, 
	INITCAP(TRIM(ir.reason_category)) return_reason_extra, 
	CASE WHEN ir.id IS NOT NULL THEN li.order_id END return_order_id, 
	INITCAP(cust.color) color, cust.size, UPPER(v.sku) style_number, 
	CASE WHEN cust.height IS NULL THEN 'Not Specified' ELSE INITCAP(cust.height) END height, 
	RANK() OVER (PARTITION BY o.email ORDER BY o.completed_at) order_num, 
	CASE WHEN tax.short_long IS NULL OR tax.short_long = 'Long,Short' 
		THEN 'Not Specified' ELSE tax.short_long END 
	short_long, 
	COALESCE(tax.skirt = 1, FALSE) skirt, 
	COALESCE(tax.evening = 1, FALSE) evening, 
	COALESCE(tax.cocktail = 1, FALSE) cocktail, 
	COALESCE(tax.bridal = 1, FALSE) bridal, 
	COALESCE(tax.daytime = 1, FALSE) daytime, 
	COALESCE(tax.jumpsuit = 1, FALSE) jumpsuit 
FROM spree_line_items li 
LEFT JOIN spree_orders o 
	ON o.id = li.order_id 
LEFT JOIN spree_variants v 
	ON v.id = li.variant_id 
LEFT JOIN spree_products p 
	ON p.id = v.product_id 
LEFT JOIN spree_addresses sa 
	ON sa.id = o.ship_address_id 
LEFT JOIN spree_states ss 
	ON ss.id = sa.state_id 
LEFT JOIN spree_countries sc 
	ON sc.id = sa.country_id 
LEFT JOIN item_returns ir 
	ON ir.line_item_id = li.id 
LEFT JOIN (
	SELECT 
		lip.line_item_id,
		MAX(CASE WHEN lip.customization_value_ids SIMILAR TO '%(1|2|3|4|5|6|7|8|9|0)%' 
				THEN 1 ELSE 0 END) physical_customization, 
		MAX(CASE WHEN pcv.custom THEN 1 ELSE 0 END) color_customization, 
		STRING_AGG(DISTINCT lip.color, ',') color, STRING_AGG(DISTINCT lip.size, ',') size, 
		STRING_AGG(DISTINCT lip.height, ',') height 
	FROM line_item_personalizations lip 
	LEFT JOIN product_color_values pcv 
	ON pcv.id = lip.color_id 
	GROUP BY line_item_id) cust 
ON cust.line_item_id = li.id 
LEFT JOIN ( 
	SELECT 
		order_id, 
		MAX(shipped_at::DATE) ship_date 
	FROM spree_shipments 
	GROUP BY order_id) s 
	ON s.order_id = li.order_id 
LEFT JOIN ( 
	SELECT   
		p.id product_id, 
		STRING_AGG(DISTINCT CASE  
			WHEN LOWER(t.name) SIMILAR TO '(mini|knee|petti)' THEN 'Short' 
			WHEN LOWER(t.name) SIMILAR TO '(midi|ankle|maxi)' THEN 'Long' 
			END, ',') short_long, 
		MAX(CASE WHEN LOWER(t.name) LIKE '%skirt%' OR LOWER(p.name) LIKE '%skirt%' 
				THEN 1 ELSE 0 END) skirt, 
		MAX(CASE WHEN t.name = 'Evening' THEN 1 ELSE 0 END) evening, 
		MAX(CASE WHEN t.name = 'Cocktail'THEN 1 ELSE 0 END) cocktail, 
		MAX(CASE WHEN LOWER(t.name) LIKE '%brid%' THEN 1 ELSE 0 END) bridal, 
		MAX(CASE WHEN t.name = 'Daytime' THEN 1 ELSE 0 END) daytime, 
		MAX(CASE WHEN LOWER(t.name) LIKE '%jump%' THEN 1 ELSE 0 END) jumpsuit 
	FROM spree_products_taxons pt JOIN spree_taxons t ON t.id = pt.taxon_id 
	INNER JOIN spree_products p 
	ON p.id = pt.product_id 
	GROUP BY p.id) tax 
	ON tax.product_id = p.id 
WHERE o.completed_at IS NOT NULL 
	AND o.completed_at >= '2016-01-01' 
	AND o.state = 'complete' 
	AND o.payment_state = 'paid')

SELECT *
FROM sales

-- with sales as (SELECT p.name, o.completed_at::DATE, o.payment_state, o.shipment_state, ir.id FROM spree_line_items li LEFT JOIN spree_orders o ON o.id = li.order_id LEFT JOIN spree_variants v ON v.id = li.variant_id LEFT JOIN spree_products p ON p.id = v.product_id LEFT JOIN spree_addresses sa ON sa.id = o.ship_address_id LEFT JOIN spree_states ss ON ss.id = sa.state_id LEFT JOIN spree_countries sc ON sc.id = sa.country_id LEFT JOIN item_returns ir ON ir.line_item_id = li.id LEFT JOIN ( SELECT lip.line_item_id, MAX(CASE WHEN lip.customization_value_ids SIMILAR TO '%(1|2|3|4|5|6|7|8|9|0)%' THEN 1 ELSE 0 END) physical_customization, MAX(CASE WHEN pcv.custom THEN 1 ELSE 0 END) color_customization, STRING_AGG(DISTINCT lip.color, ',') color, STRING_AGG(DISTINCT lip.size, ',') size, STRING_AGG(DISTINCT lip.height, ',') height FROM line_item_personalizations lip LEFT JOIN product_color_values pcv ON pcv.id = lip.color_id GROUP BY line_item_id) cust ON cust.line_item_id = li.id LEFT JOIN ( SELECT order_id, MAX(shipped_at::DATE) ship_date FROM spree_shipments GROUP BY order_id) s ON s.order_id = li.order_id LEFT JOIN ( SELECT   p.id product_id, STRING_AGG(DISTINCT CASE  WHEN LOWER(t.name) SIMILAR TO '(mini|knee|petti)' THEN 'Short' WHEN LOWER(t.name) SIMILAR TO '(midi|ankle|maxi)' THEN 'Long' END, ',') short_long, MAX(CASE WHEN LOWER(t.name) LIKE '%skirt%' OR LOWER(p.name) LIKE '%skirt%' THEN 1 ELSE 0 END) skirt, MAX(CASE WHEN t.name = 'Evening' THEN 1 ELSE 0 END) evening, MAX(CASE WHEN t.name = 'Cocktail'THEN 1 ELSE 0 END) cocktail, MAX(CASE WHEN LOWER(t.name) LIKE '%brid%' THEN 1 ELSE 0 END) bridal, MAX(CASE WHEN t.name = 'Daytime' THEN 1 ELSE 0 END) daytime, MAX(CASE WHEN LOWER(t.name) LIKE '%jump%' THEN 1 ELSE 0 END) jumpsuit FROM spree_products_taxons pt JOIN spree_taxons t ON t.id = pt.taxon_id JOIN spree_products p ON p.id = pt.product_id GROUP BY p.id) tax ON tax.product_id = p.id WHERE o.completed_at IS NOT NULL AND o.completed_at >= '2016-01-01' AND o.state = 'complete')
-- select * from sales
