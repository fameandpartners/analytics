SELECT CASE
           WHEN rri.id IS NOT NULL THEN 'YES'
           ELSE 'NO'
       END refund_requested,
       COUNT(*)
FROM spree_line_items li
INNER JOIN spree_orders o ON o.id = li.order_id
LEFT JOIN return_request_items rri ON rri.line_item_id = li.id
WHERE o.completed_at IS NOT NULL
  AND o.payment_state = 'paid'
  AND o.completed_at >= '2016-01-01'
GROUP BY CASE
             WHEN rri.id IS NOT NULL THEN 'YES'
             ELSE 'NO'
         END