-- Ben Smith <bens@fameandpartners.com>	Fri, Jan 20, 2017 at 11:26 AM
-- To: Peter Morrow <peterm@fameandpartners.com>
-- Hi Peter,
-- Hope you’re settling in well.
-- Do you have access to pull the returns information including reason codes and notes etc? 
-- I’d love to get the last 6 months of data if you can pull it?
-- Thanks,
-- Ben

SELECT 
	ir.line_item_id, 
	li.order_id,
	ir.order_number,
	ir.refund_amount / 100 refund_amount,
	li.price * li.quantity original_amount,
	ir.customer_name,
	ir.contact_email,
	ir.product_name,
	ir.reason_category,
	ir.reason_sub_category,
	ir.request_notes,
	ir.refund_status,
	lip.color,
	lip.size,
	lip.height,
	lip.line_item_personalization_id,
	o.completed_at::date order_date,
	s.shipment_date,
	ir.refunded_at::date refund_date
FROM item_returns ir
INNER JOIN spree_line_items li
	ON li.id = ir.line_item_id
INNER JOIN spree_orders o
	ON o.id = li.order_id
INNER JOIN (
	SELECT order_id, MAX(shipped_at::date) shipment_date
	FROM spree_shipments
	GROUP BY order_id) s
	ON s.order_id = o.id
LEFT JOIN (
	SELECT 
		line_item_id, 
		STRING_AGG(DISTINCT color, ',') color,
		STRING_AGG(DISTINCT size, ',') size,
		STRING_AGG(DISTINCT height, ',') height,
		MIN(id) line_item_personalization_id
	FROM line_item_personalizations
	GROUP BY line_item_id) lip
	ON lip.line_item_id = li.id
WHERE o.completed_at IS NOT NULL
	AND ir.requested_action = 'return'
	AND o.completed_at >= '2016-06-01';

-- \copy (Select * From foo) To '/tmp/test.csv' With CSV
-- \copy (SELECT   ir.line_item_id,   li.order_id,  ir.order_number,  ir.refund_amount / 100 refund_amount,  li.price * li.quantity original_amount,  ir.customer_name,  ir.contact_email,  ir.product_name,ir.reason_category,  ir.reason_sub_category,  ir.request_notes,  ir.refund_status,  lip.color,  lip.size,  lip.height, lip.line_item_personalization_id,  o.completed_at::date order_date,  s.shipment_date,  ir.refunded_at::date refund_date FROM item_returns ir INNER JOIN spree_line_items li  ON li.id = ir.line_item_id INNER JOIN spree_orders o  ON o.id = li.order_id INNER JOIN (  SELECT order_id, MAX(shipped_at::date) shipment_date  FROM spree_shipments  GROUP BY order_id) s  ON s.order_id = o.id LEFT JOIN (  SELECT     line_item_id,     STRING_AGG(DISTINCT color, ',') color,    STRING_AGG(DISTINCT size, ',') size,    STRING_AGG(DISTINCT height, ',') height, MIN(id) line_item_personalization_id  FROM line_item_personalizations  GROUP BY line_item_id) lip  ON lip.line_item_id = li.id WHERE o.completed_at IS NOT NULL  AND ir.requested_action = 'return'  AND o.completed_at >= '2016-06-01') to 'returns since Jun 2016.csv' with csv

