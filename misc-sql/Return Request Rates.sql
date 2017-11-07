SELECT EXTRACT(YEAR
               FROM o.completed_at) order_year,
       COUNT(*) units_ordered,
       SUM(CASE WHEN rri.id IS NOT NULL THEN 1 ELSE 0 END) return_requests,
       SUM(CASE WHEN rri.id IS NOT NULL THEN 1.0 ELSE 0.0 END) / COUNT(*) return_request_rate
FROM spree_line_items li
JOIN spree_orders o ON o.id = li.order_id
LEFT JOIN return_request_items rri ON li.id = rri.line_item_id
WHERE o.completed_at IS NOT NULL
  AND o.total > 0
GROUP BY EXTRACT(YEAR
                 FROM o.completed_at)