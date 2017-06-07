DELETE FROM spree_orders WHERE id IN (
	SELECT id
	FROM spree_orders o
	LEFT JOIN spree_line_items li
		ON li.order_id = o.id
	WHERE o.completed_at IS NULL
		AND li.id IS NULL
)