SELECT 
	EXTRACT(YEAR FROM o.completed_at) order_year, 
	SUM(o.adjustment_total) total_adjustments,
	SUM(CASE WHEN o.total = 0 THEN 1 ELSE 0 END) gifted_free_orders
FROM spree_orders o 
WHERE o.completed_at IS NOT NULL
	AND o.payment_state = 'paid'
GROUP BY order_year
ORDER BY order_year, gifted_free_orders, total_adjustments

SELECT
	EXTRACT(YEAR FROM o.completed_at) order_year,
	a.label promotion_code,
	SUM(a.amount) total_adjustments,
	COUNT(DISTINCT CASE WHEN o.total = 0 THEN o.id END) gifted_free_orders
FROM spree_orders o 
LEFT JOIN spree_adjustments a
	ON o.id = a.adjustable_id
WHERE o.completed_at IS NOT NULL
	AND o.payment_state = 'paid'
GROUP BY order_year, a.label
ORDER BY order_year, total_adjustments

-- SELECT EXTRACT(YEAR FROM a.created_at) order_year, a.label, SUM(a.amount) total_adjustments, COUNT(DISTINCT CASE WHEN o.total = 0 THEN o.id END) gifted_free_orders FROM spree_adjustments a INNER JOIN spree_orders o ON o.id = a.adjustable_id WHERE a.amount < 0  AND o.completed_at IS NOT NULL AND o.payment_state = 'paid' GROUP BY order_year, a.label ORDER BY order_year, total_adjustments
