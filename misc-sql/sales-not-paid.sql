SELECT o.id,
       o.payment_state payment_state_o,
       o.currency,
       p.state payment_state_p,
       COUNT(*) payments,
       SUM(o.total) order_total,
       SUM(p.amount) payment_total
FROM spree_orders o
LEFT JOIN spree_payments p ON p.order_id = o.id
WHERE o.completed_at IS NOT NULL
  AND o.payment_state != 'paid'
GROUP BY o.id,
         o.payment_state,
         o.currency,
         p.state
ORDER BY o.id -- SELECT o.id, o.payment_state payment_state_o, o.currency, p.state payment_state_p, COUNT(*) payments, SUM(o.total) order_total, SUM(p.amount) payment_total FROM spree_orders o LEFT JOIN spree_payments p ON p.order_id = o.id WHERE o.completed_at IS NOT NULL AND o.payment_state != 'paid' GROUP BY o.id, o.payment_state, o.currency, p.state ORDER BY o.id