SELECT EXTRACT(YEAR
               FROM COALESCE(s.ship_date, o.completed_at::DATE + 10)) order_year,
       SUM(CASE WHEN o.currency = 'AUD' THEN 0.77 ELSE 1 END * o.total) revenue,
       SUM(CASE WHEN o.currency = 'AUD' THEN 0.77 ELSE 1 END * p.payment_amount) payments
FROM spree_orders o
LEFT JOIN
  ( SELECT order_id,
           MAX(shipped_at::DATE) ship_date
   FROM spree_shipments
   GROUP BY order_id) s ON s.order_id = o.id
LEFT JOIN
  ( SELECT order_id,
           SUM(amount) payment_amount
   FROM spree_payments -- WHERE state = 'completed'

   GROUP BY order_id) p ON p.order_id = o.id
WHERE o.completed_at IS NOT NULL
  AND o.payment_state = 'paid'
  AND COALESCE(s.ship_date, o.completed_at::DATE + 10) >= '2015-01-01'
GROUP BY order_year
ORDER BY order_year