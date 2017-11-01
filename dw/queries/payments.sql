SELECT order_id, amount p_amount
FROM spree_payments
WHERE state = 'completed'
