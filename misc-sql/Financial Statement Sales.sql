-- Using 0.77 to convert from USD to AUD

SELECT
	EXTRACT(YEAR FROM COALESCE(s.ship_date, o.completed_at::DATE + 10)) order_year,
	SUM(CASE WHEN o.currency = 'AUD'
		THEN 0.77 ELSE 1 END * o.total) revenue
FROM spree_orders o
LEFT JOIN (
	SELECT order_id, MAX(shipped_at::DATE) ship_date
	FROM spree_shipments
	GROUP BY order_id
) s ON s.order_id = o.id
WHERE o.completed_at IS NOT NULL
	AND o.payment_state = 'paid'
	AND COALESCE(s.ship_date, o.completed_at::DATE + 10) >= '2015-01-01'
GROUP BY order_year
ORDER BY order_year

--  order_year |   revenue
-- ------------+--------------
--        2015 | 2404259.5528
--        2016 | 3900723.6344
--        2017 | 1375002.1703


-- Monthly
SELECT
	EXTRACT(YEAR FROM COALESCE(s.ship_date, o.completed_at::DATE + 10)) order_year,
	EXTRACT(MONTH FROM COALESCE(s.ship_date, o.completed_at::DATE + 10)) order_month,
	SUM(CASE WHEN o.currency = 'AUD'
		THEN 0.74342 ELSE 1 END * o.total) revenue
FROM spree_orders o
LEFT JOIN (
	SELECT order_id, MAX(shipped_at::DATE) ship_date
	FROM spree_shipments
	GROUP BY order_id
) s ON s.order_id = o.id
WHERE o.completed_at IS NOT NULL
	AND o.state != 'canceled'
--	AND o.payment_state = 'paid'
	AND COALESCE(s.ship_date, o.completed_at::DATE + 10) >= '2015-01-01'
GROUP BY order_year, order_month
ORDER BY order_year, order_month
