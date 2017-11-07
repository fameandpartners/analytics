SELECT p.name product_name,
       SUM(CASE WHEN ir.id IS NOT NULL THEN li.quantity::FLOAT ELSE 0.0 END) / SUM(li.quantity::FLOAT) return_rate,
                                                                               SUM(li.quantity) items_sold
FROM spree_line_items li
INNER JOIN spree_orders o ON o.id = li.order_id
INNER JOIN spree_variants v ON v.id = li.variant_id
INNER JOIN spree_products p ON p.id = v.product_id
LEFT JOIN item_returns ir ON ir.line_item_id = li.id
WHERE o.completed_at IS NOT NULL
  AND o.completed_at >= '2016-01-01'
GROUP BY p.name HAVING SUM(li.quantity) > 10
ORDER BY SUM(li.quantity) DESC --\copy (SELECT p.name product_name, SUM(CASE WHEN ir.id IS NOT NULL THEN li.quantity::FLOAT ELSE 0.0 END) / SUM(li.quantity::FLOAT) return_rate, SUM(li.quantity) items_sold FROM spree_line_items li INNER JOIN spree_orders o  ON o.id = li.order_id INNER JOIN spree_variants v ON v.id = li.variant_id INNER JOIN spree_products p  ON p.id = v.product_id LEFT JOIN item_returns ir ON ir.line_item_id = li.id WHERE o.completed_at IS NOT NULL AND o.completed_at >= '2016-01-01' GROUP BY p.name HAVING SUM(li.quantity) > 10 ORDER BY SUM(li.quantity) DESC) to '2016 product return rates.csv' with csv