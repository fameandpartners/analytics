-- Nov 22, 2016 to 31 Dec, 2016

SELECT number, completed_at
FROM spree_orders
WHERE completed_at IS NOT NULL
  AND total > 0
  AND completed_at BETWEEN '2016-11-22' AND '2016-12-31' -- SELECT number, completed_at FROM spree_orders WHERE completed_at IS NOT NULL AND total > 0 AND completed_at BETWEEN '2016-11-22' AND '2016-12-31'
 -- Queried on 2017-04-18
-- Jan 1 - March 31, 2017
 -- Excel 1) All your confirmed transactions: order id, date and timestamp

  SELECT number, completed_at
  FROM spree_orders WHERE completed_at IS NOT NULL
  AND total > 0
  AND completed_at BETWEEN '2017-01-01' AND '2017-03-31'
  AND STATE != 'canceled'
  AND number NOT LIKE 'E%' -- SELECT number, completed_at FROM spree_orders WHERE completed_at IS NOT NULL AND total > 0 AND completed_at BETWEEN '2017-01-01' AND '2017-03-31' AND state != 'canceled' AND number NOT LIKE 'E%'
 -- Excel 2) Transactions where all items were returned: order id, date and timestamp
-- We cannot ID which orders have had a subset of their items returned because of bad data so
-- we will include all orders that have at least 1 returned item

  SELECT DISTINCT o.number, o.completed_at
  FROM spree_line_items li
  INNER JOIN spree_orders o ON li.order_id = o.id
  INNER JOIN item_returns ir ON ir.line_item_id = li.id WHERE o.completed_at IS NOT NULL
  AND o.total > 0
  AND o.completed_at BETWEEN '2017-01-01' AND '2017-03-31'
  AND o.STATE != 'canceled'
  AND o.number NOT LIKE 'E%' -- SELECT DISTINCT o.number, o.completed_at FROM spree_line_items li INNER JOIN spree_orders o ON li.order_id = o.id INNER JOIN item_returns ir ON ir.line_item_id = li.id WHERE o.completed_at IS NOT NULL AND o.total > 0 AND o.completed_at BETWEEN '2017-01-01' AND '2017-03-31' AND o.state != 'canceled' AND o.number NOT LIKE 'E%'
 -- Pulled for iequalchange on 2017-08-23

  SELECT DISTINCT o.number, o.completed_at
  FROM spree_line_items li
  INNER JOIN spree_orders o ON li.order_id = o.id
  INNER JOIN item_returns ir ON ir.line_item_id = li.id WHERE o.completed_at IS NOT NULL
  AND o.total > 0
  AND o.completed_at BETWEEN '2017-04-01' AND '2017-06-30'
  AND o.STATE != 'canceled'
  AND o.number NOT LIKE 'E%'