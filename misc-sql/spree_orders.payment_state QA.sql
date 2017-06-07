SELECT o.payment_state, p.states_in_spree_payments, COUNT(*)
FROM spree_orders o
LEFT JOIN (
	SELECT order_id, STRING_AGG(DISTINCT state, ',') states_in_spree_payments
	FROM spree_payments
	GROUP BY order_id
) p ON p.order_id = o.id
WHERE o.completed_at IS NOT NULL
	AND o.payment_state = 'paid'
	AND o.completed_at >= '2016-01-01'
GROUP BY o.payment_state, p.states_in_spree_payments

--  payment_state |  states_in_spree_payments   | count
-- ---------------+-----------------------------+-------
--  paid          | completed,void              |     1
--  paid          | checkout,failed             |     1 **
--  paid          | checkout                    |    29 **
--  paid          | completed                   | 16949
--  paid          | completed,failed,processing |     3
--  paid          |                             |  1652 ** 
--  paid          | completed,failed            |  1408
--  paid          | completed,processing        |    10
--  paid          | checkout,completed          |     2

SELECT p.states_in_spree_payments, o.payment_state, COUNT(*)
FROM (
	SELECT order_id, STRING_AGG(DISTINCT state, ',') states_in_spree_payments
	FROM spree_payments
	WHERE state = 'completed'
	GROUP BY order_id
) p
LEFT JOIN spree_orders o
	ON p.order_id = o.id
WHERE o.completed_at IS NOT NULL
	AND o.completed_at >= '2016-01-01'
GROUP BY p.states_in_spree_payments, o.payment_state

--  states_in_spree_payments | payment_state | count
-- --------------------------+---------------+-------
--  completed                | failed        |     2 **
--  completed                | paid          | 18374
--  completed                | balance_due   |     8 **
--  completed                | credit_owed   |    71 **

SELECT o.* FROM spree_orders o LEFT JOIN ( SELECT order_id, STRING_AGG(DISTINCT state, ',') states_in_spree_payments FROM spree_payments GROUP BY order_id ) p ON p.order_id = o.id WHERE o.completed_at IS NOT NULL AND o.payment_state = 'paid' AND o.completed_at >= '2016-01-01' AND p.states_in_spree_payments IS NULL

SELECT o.* FROM spree_orders o LEFT JOIN ( SELECT order_id, STRING_AGG(DISTINCT state, ',') states_in_spree_payments FROM spree_payments GROUP BY order_id ) p ON p.order_id = o.id WHERE o.completed_at IS NOT NULL AND o.payment_state = 'paid' AND o.completed_at >= '2016-01-01' AND p.states_in_spree_payments IN ('checkout,failed','checkout')

select o.* FROM ( SELECT order_id, STRING_AGG(DISTINCT state, ',') states_in_spree_payments FROM spree_payments WHERE state = 'completed' GROUP BY order_id) p LEFT JOIN spree_orders o ON p.order_id = o.id WHERE o.completed_at IS NOT NULL AND o.completed_at >= '2016-01-01' AND o.payment_state = 'credit_owed';