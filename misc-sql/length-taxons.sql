 -- spot check taxon data
-- * any issues? mutiple lengths?
-- * how much of sales do we have tagged?
 -- products with taxon

SELECT p.id product_id,
       STRING_AGG(DISTINCT LOWER(TRIM(t.name)), ',') lengths
FROM spree_products_taxons pt
JOIN spree_taxons t ON t.id = pt.taxon_id
JOIN spree_products p ON p.id = pt.product_id
WHERE t.name SIMILAR TO '%(maxi|midi|mini|knee|petti|ankle)%'
GROUP BY p.id -- sales by length taxon

SELECT EXTRACT(YEAR
               FROM o.completed_at) order_year,
       LENGTH,
       COUNT(*) units
FROM spree_line_items li
LEFT JOIN spree_orders o ON o.id = li.order_id
LEFT JOIN spree_variants v ON v.id = li.variant_id
LEFT JOIN
  ( SELECT pt.product_id product_id,
           STRING_AGG(DISTINCT LOWER(TRIM(t.name)), ',') LENGTH
   FROM spree_products_taxons pt
   JOIN spree_taxons t ON t.id = pt.taxon_id
   WHERE LOWER(TRIM(t.name)) SIMILAR TO '%(midi|mini|knee|petti|ankle|long)%'
     AND t.name != 'Long Sleeve'
   GROUP BY pt.product_id) len ON len.product_id = v.product_id
WHERE o.completed_at IS NOT NULL
  AND o.completed_at >= '2016-01-01'
  AND o.payment_state = 'paid'
GROUP BY EXTRACT(YEAR
                 FROM o.completed_at),
         LENGTH
ORDER BY EXTRACT(YEAR
                 FROM o.completed_at),
         LENGTH -- order_year |   length   | units
-- ------------+------------+-------
--        2016 | ankle      |   166
--        2016 | knee       |    61
--        2016 | knee,Midi  |     5
--        2016 | knee,Mini  |    10
--        2016 | knee,petti |     4
--        2016 | Maxi       |  5069
--        2016 | Midi       |   504
--        2016 | Midi,Mini  |     2
--        2016 | Midi,petti |    41
--        2016 | Mini       |   460
--        2016 | petti      |   390
--        2016 |            | 11475
--        2017 | ankle      |    16
--        2017 | knee       |     2
--        2017 | knee,Midi  |     1
--        2017 | Maxi       |   992
--        2017 | Midi       |    69
--        2017 | Midi,petti |     4
--        2017 | Mini       |   116
--        2017 | petti      |    69
--        2017 |            |  3350

SELECT p.id,
       p.name,
       len.LENGTH,
           MAX(o.completed_at::DATE) most_recent_sale_date
FROM spree_line_items li
LEFT JOIN spree_orders o ON o.id = li.order_id
LEFT JOIN spree_variants v ON v.id = li.variant_id
LEFT JOIN spree_products p ON p.id = v.product_id
LEFT JOIN
  ( SELECT pt.product_id product_id,
           STRING_AGG(DISTINCT LOWER(TRIM(t.name)), ',') LENGTH
   FROM spree_products_taxons pt
   JOIN spree_taxons t ON t.id = pt.taxon_id
   WHERE LOWER(TRIM(t.name)) SIMILAR TO '%(midi|mini|knee|petti|ankle|long)%'
     AND t.name != 'Long Sleeve'
   GROUP BY pt.product_id) len ON len.product_id = v.product_id
WHERE o.completed_at IS NOT NULL
  AND o.completed_at >= '2016-01-01'
  AND o.payment_state = 'paid'
GROUP BY p.id,
         p.name,
         len.LENGTH -- SELECT p.id, p.name, len.length, MAX(o.completed_at::DATE) most_recent_sale_date FROM spree_line_items li  LEFT JOIN spree_orders o  ON o.id = li.order_id  LEFT JOIN spree_variants v  ON v.id = li.variant_id  LEFT JOIN spree_products p ON p.id = v.product_id LEFT JOIN ( SELECT pt.product_id product_id, STRING_AGG(DISTINCT LOWER(TRIM(t.name)), ',') length FROM spree_products_taxons pt JOIN spree_taxons t ON t.id = pt.taxon_id WHERE LOWER(TRIM(t.name)) SIMILAR TO '%(midi|mini|knee|petti|ankle|long)%'  AND t.name != 'Long Sleeve' GROUP BY pt.product_id ) len ON len.product_id = v.product_id WHERE o.completed_at IS NOT NULL  AND o.completed_at >= '2016-01-01'  AND o.payment_state = 'paid' GROUP BY p.id, p.name, len.length