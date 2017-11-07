-- How many orders marked as completed and paid have no payment record?

SELECT COUNT(DISTINCT o.id) completed_orders,
       COUNT(DISTINCT CASE WHEN p.id IS NULL THEN o.id END) missing_payment
FROM spree_orders o
LEFT JOIN spree_payments p ON o.id = p.order_id
WHERE o.state = 'complete'
  AND o.payment_state = 'paid'
  AND o.shipment_state = 'shipped'
  AND o.completed_at IS NOT NULL;

 --  completed_orders | missing_payment
-- ------------------+-----------------
--             26073 |            1212

SELECT EXTRACT(YEAR
               FROM o.completed_at) completed_year,
       EXTRACT(MONTH
               FROM o.completed_at) completed_month,
       COUNT(DISTINCT o.id) completed_orders,
       COUNT(DISTINCT CASE WHEN p.id IS NULL THEN o.id END) missing_payment
FROM spree_orders o
LEFT JOIN spree_payments p ON o.id = p.order_id
WHERE o.state = 'complete'
  AND o.payment_state = 'paid'
  AND o.shipment_state = 'shipped'
  AND o.completed_at IS NOT NULL
GROUP BY EXTRACT(YEAR
                 FROM o.completed_at),
         EXTRACT(MONTH
                 FROM o.completed_at);