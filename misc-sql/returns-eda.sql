-- 75% of orders in item_returns are in order_return_requests

SELECT COUNT(DISTINCT ir.order_number) ir_orders,
       COUNT(DISTINCT orr.order_number) orr_orders
FROM item_returns ir
LEFT JOIN
  ( SELECT DISTINCT o.number order_number
   FROM order_return_requests orr
   INNER JOIN spree_orders o ON o.id = orr.order_id) orr ON orr.order_number = ir.order_number;

 --  ir_orders | orr_orders
-- -----------+------------
--       8652 |       6494
 -- 97% of orders in order_return_requests are in item_returns

SELECT COUNT(DISTINCT o.number) orr_orders,
       COUNT(DISTINCT ir.order_number) ir_orders
FROM order_return_requests orr
INNER JOIN spree_orders o ON o.id = orr.order_id
LEFT JOIN
  ( SELECT DISTINCT order_number
   FROM item_returns) ir ON ir.order_number = o.number;

 --  orr_orders | ir_orders
-- ------------+-----------
--        6705 |      6494
