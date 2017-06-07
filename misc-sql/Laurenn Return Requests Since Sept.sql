SELECT
    rri.line_item_id,
    o.number order_number,
    rri.reason_category,
    rri.reason,
    o.completed_at::DATE order_date,
    rri.created_at::DATE return_request_date
FROM return_request_items rri
INNER JOIN spree_line_items li
    ON li.id = rri.line_item_id
INNER JOIN spree_orders o
    ON o.id = li.order_id
WHERE o.completed_at IS NOT NULL
    AND rri.action IN ('exchange','return')
    AND rri.created_at >= '2016-09-01'

-- SELECT rri.line_item_id, o.number order_number, rri.reason_category, rri.reason, o.completed_at::DATE order_date, rri.created_at::DATE return_request_date FROM return_request_items rri INNER JOIN spree_line_items li ON li.id = rri.line_item_id INNER JOIN spree_orders o ON o.id = li.order_id WHERE o.completed_at IS NOT NULL AND rri.action IN ('exchange','return') AND rri.created_at >= '2016-09-01'
