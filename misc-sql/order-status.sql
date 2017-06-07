SELECT
	CASE 
		WHEN o.state != 'canceled' AND (o.shipment_state IS NULL OR o.shipment_state = 'partial') THEN 'Paid'
		WHEN o.state != 'canceled' THEN INITCAP(o.shipment_state)
		WHEN r.items_returned IS NOT NULL THEN 'Returned'
		ELSE INITCAP(o.state)
	END order_status,
	COUNT(*)
FROM spree_orders o
LEFT JOIN (
	SELECT li.order_id, COUNT(*) items_returned
	FROM item_returns ir
	INNER JOIN spree_line_items li 
		ON ir.line_item_id = li.id
	GROUP BY li.order_id) r
	ON r.order_id = o.id
WHERE o.completed_at IS NOT NULL
	AND o.payment_state = 'paid'
GROUP BY 
	CASE 
		WHEN o.state != 'canceled' AND (o.shipment_state IS NULL OR o.shipment_state = 'partial') THEN 'Paid'
		WHEN o.state != 'canceled' THEN INITCAP(o.shipment_state)
		WHEN r.items_returned IS NOT NULL THEN 'Returned'
		ELSE INITCAP(o.state)
	END