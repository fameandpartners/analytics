SELECT 
	EXTRACT(YEAR FROM completed_at) order_year,
	EXTRACT(MONTH FROM completed_at) order_month,
	SUM(total) cash
FROM spree_orders
WHERE completed_at IS NOT NULL
	AND payment_state = 'paid'
GROUP BY
	EXTRACT(YEAR FROM completed_at),
	EXTRACT(MONTH FROM completed_at)
ORDER BY
	EXTRACT(YEAR FROM completed_at),
	EXTRACT(MONTH FROM completed_at)