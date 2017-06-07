SELECT ir.id item_return_id, ir.requested_at, ir.uuid, ir.request_id
FROM item_returns ir 
LEFT JOIN bergen_return_item_processes bp 
	ON ir.request_id = bp.return_request_item_id
WHERE bp.return_request_item_id IS NULL 
	AND ir.requested_at BETWEEN '2017-01-01' AND CURRENT_DATE
	AND ir.order_paid_currency = 'USD'

-- SELECT ir.id item_return_id, ir.requested_at, ir.uuid, ir.request_id FROM item_returns ir  LEFT JOIN bergen_return_item_processes bp  ON ir.request_id = bp.return_request_item_id WHERE bp.return_request_item_id IS NULL  AND ir.requested_at BETWEEN '2017-01-01' AND CURRENT_DATE AND ir.order_paid_currency = 'USD'
