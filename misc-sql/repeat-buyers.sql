-- Who are our repeat customers
-- Age
-- Geo
-- Profession 
-- Ethnicity
-- Purchase reason (for each purchase) - i.e. event, etc
-- Return reasons

-- What did they buy
-- Categorisation of styles to understand in a graph what they actually purchased
-- Short VS long
-- Apparel category (from website)
-- Skirt
-- Evening gown
-- Cocktail dress
-- Day dress
-- Jumpsuit
-- Bridesmaid dress
-- By collection
-- By style (this is already in there but what I can’t see easily is this data by first and second purchase)
-- Colours
-- Colours against time of year (trying to understand relationship between seasons and colours)
-- Sizes
-- By first purchase and second purchase
-- LTV
-- By age
-- By Geo
-- By Profession

-- The likelihood is that we may not be able to do the meeting tomorrow (because I have to do an urgent call with Macy’s) but I’m hopeful that above is enough direction for you to now take the next step. I understand that it may require an additional survey or similar but let me know what you think you can pull together with what you have / can derive from steps like reviewing people’s social media accounts.

SELECT 
	EXTRACT(YEAR FROM s.ship_date) ship_year,
	EXTRACT(MONTH FROM s.ship_date) ship_month,
	COUNT(*) orders,
	SUM(CASE WHEN which_order > 1 THEN 1 ELSE 0 END) repeat_orders,
	COUNT(DISTINCT email) total_customers,
	COUNT(DISTINCT CASE WHEN which_order > 1 THEN email ELSE NULL END) returning_customers
FROM
	(SELECT *, RANK() OVER (PARTITION BY email ORDER BY completed_at) which_order
	FROM spree_orders
	WHERE completed_at IS NOT NULL) o
INNER JOIN (
	SELECT 
		order_id, 
		MAX(shipped_at::DATE) ship_date 
	FROM spree_shipments 
	GROUP BY order_id) s 
	ON s.order_id = o.id 
WHERE s.ship_date IS NOT NULL
	AND s.ship_date >= '2016-01-01'
GROUP BY ship_year, ship_month
ORDER BY ship_year, ship_month

SELECT 
	li.id, 
	li.order_id, 
	li.quantity, 
	li.price, 
	li.currency, 
	s.ship_date, o.email, o.user_id, 
	INITCAP(o.user_first_name) || ' ' || INITCAP(o.user_last_name) customer_name, 
	p.id product_id, INITCAP(p.name) style_name, 
	ir.id IS NOT NULL item_returned, 
	INITCAP(TRIM(ir.reason_category)) return_reason, 
	CASE WHEN ir.id IS NOT NULL THEN li.order_id END return_order_id, 
	INITCAP(sa.city) shipping_city, 
	INITCAP(ss.name) shipping_state, 
	INITCAP(sc.name) shipping_country, 
	INITCAP(cust.color) color, 
	cust.size, 
	cust.height,
	o.completed_at::date order_date, 
	MIN(o.completed_at::date) OVER (PARTITION BY o.email) first_order_date, 
	MAX(o.completed_at::date) OVER (PARTITION BY o.email) last_order_date,
	RANK() OVER (PARTITION BY o.email ORDER BY o.completed_at) which_order,
	RANK() OVER (PARTITION BY o.email, p.id ORDER BY o.completed_at) same_product_order
FROM spree_line_items li 
LEFT JOIN item_returns ir 
	ON ir.line_item_id = li.id 
INNER JOIN spree_orders o 
	ON o.id = li.order_id 
INNER JOIN spree_variants v 
	ON v.id = li.variant_id 
INNER JOIN spree_products p 
	ON p.id = v.product_id 
LEFT JOIN spree_addresses sa 
	ON sa.id = o.ship_address_id 
LEFT JOIN spree_states ss 
	ON ss.id = sa.state_id 
LEFT JOIN spree_countries sc 
	ON sc.id = sa.country_id 
LEFT JOIN (
	SELECT 
		line_item_id, 
		STRING_AGG(DISTINCT color, ',') color, 
		STRING_AGG(DISTINCT size, ',') size, 
		STRING_AGG(DISTINCT height, ',') height 
	FROM line_item_personalizations 
	GROUP BY line_item_id) cust 
	ON cust.line_item_id = li.id 
LEFT JOIN (
	SELECT 
		order_id, 
		MAX(shipped_at::DATE) ship_date 
	FROM spree_shipments 
	GROUP BY order_id) s 
	ON s.order_id = li.order_id 
WHERE o.completed_at IS NOT NULL
	AND o.completed_at >= '2016-06-01' 
	AND o.state = 'complete' 
	AND o.payment_state = 'paid' 
	AND o.shipment_state = 'shipped' 
	AND o.email NOT LIKE '%fameandpartners.com' 
	AND o.email != 'fameprteam@gmail.com'



SELECT o.created_at::DATE, s.ship_date, o.projected_delivery_date
FROM spree_orders o
LEFT JOIN (
	SELECT 
		order_id, 
		MAX(shipped_at::DATE) ship_date 
	FROM spree_shipments 
	GROUP BY order_id) s 
	ON s.order_id = o.id
WHERE s.ship_date IS NULL 