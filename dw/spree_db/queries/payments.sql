SELECT order_id,
       amount p_amount
FROM spree_payments
WHERE STATE = 'completed'
  AND created_at >= '2015-12-01'