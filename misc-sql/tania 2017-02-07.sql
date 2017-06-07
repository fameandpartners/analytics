SELECT 
	p.name product_name,
	fs.last_sale_date,
	p.available_on,
	p.created_at::DATE
FROM spree_products p
INNER JOIN 
	(SELECT 
		v.product_id,
		MAX(o.completed_at)::DATE last_sale_date,
		COUNT(*) items_sold
	FROM spree_line_items li
	INNER JOIN spree_orders o 
		ON o.id = li.order_id
	INNER JOIN spree_variants v
		ON v.id = li.variant_id
	WHERE o.completed_at IS NOT NULL
		AND o.state = 'complete'
		AND o.payment_state = 'paid'
	GROUP BY 
		v.product_id) fs
	ON fs.product_id = p.id
WHERE fs.last_sale_date < '2016-07-01'
	-- active products
	AND NOT p.hidden
	AND (p.deleted_at IS NULL OR p.deleted_at >= CURRENT_DATE)
	AND (p.available_on <= CURRENT_DATE)

-- \copy (SELECT  p.name product_name, fs.last_sale_date, p.available_on, p.created_at::DATE FROM spree_products p INNER JOIN  (SELECT  v.product_id, MAX(o.completed_at)::DATE last_sale_date, COUNT(*) items_sold FROM spree_line_items li INNER JOIN spree_orders o  ON o.id = li.order_id INNER JOIN spree_variants v ON v.id = li.variant_id WHERE o.completed_at IS NOT NULL AND o.state = 'complete' AND o.payment_state = 'paid' GROUP BY  v.product_id) fs ON fs.product_id = p.id WHERE fs.last_sale_date < '2016-07-01' AND NOT p.hidden AND (p.deleted_at IS NULL OR p.deleted_at >= CURRENT_DATE) AND (p.available_on <= CURRENT_DATE)) to 'tania 2017-02-07.csv' with csv

-- SELECT "spree_products".* FROM "spree_products" INNER JOIN "spree_variants" ON "spree_variants"."product_id" = "spree_products"."id" AND "spree_variants"."is_master" = 't' INNER JOIN "spree_prices" ON "spree_prices"."variant_id" = "spree_variants"."id" WHERE "spree_products"."hidden" = 'f' AND "spree_prices"."currency" = 'AUD' AND ("spree_products".deleted_at IS NULL or "spree_products".deleted_at >= '2017-02-07 21:19:19.264862') AND ("spree_products".available_on <= '2017-02-07 21:19:19.265458') AND (spree_prices.amount IS NOT NULL) ORDER BY spree_products.position"