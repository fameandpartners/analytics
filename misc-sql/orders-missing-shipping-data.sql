-- Missing Ship Date

SELECT o.id order_id,
       o.number order_number,
       o.completed_at::DATE order_date,
       s.tracking_numbers,
       s.ship_date
FROM spree_orders o
LEFT JOIN
  ( SELECT order_id,
           STRING_AGG(DISTINCT number, ',') tracking_numbers,
           MAX(shipped_at::DATE) ship_date
   FROM spree_shipments
   GROUP BY order_id) s ON s.order_id = o.id
WHERE o.completed_at IS NOT NULL
  AND o.shipment_state = 'shipped'
  AND s.ship_date IS NULL
ORDER BY order_date;

 -- \copy (SELECT o.id order_id, o.number order_number, o.completed_at::DATE order_date, s.tracking_numbr, s.ship_date FROM spree_orders o LEFT JOIN (SELECT order_id, STRING_AGG(DISTINCT number, ',') tracking_numbr, MAX(shipped_at::DATE) ship_date FROM spree_shipments GROUP BY order_id) s ON s.order_id = o.id WHERE o.completed_at IS NOT NULL AND o.shipment_state = 'shipped' AND s.ship_date IS NULL ORDER BY order_date) to 'orders missing ship date 2017-01-31.csv' with csv
 -- Missing Tracking Number

SELECT o.id order_id,
       o.number order_number,
                o.completed_at::DATE order_date,
                s.spree_tracking_numbers,
                s.tracking_numbers,
                s.ship_date
FROM spree_orders o
INNER JOIN
  ( SELECT order_id,
           STRING_AGG(DISTINCT tracking, ',') tracking_numbers,
           STRING_AGG(DISTINCT number, ',') spree_tracking_numbers,
           MAX(shipped_at::DATE) ship_date
   FROM spree_shipments
   WHERE tracking IS NULL
     OR (tracking SIMILAR TO '%(a|b|c|d|e|f|g|h|i|j|k|l|m|n|o|p|q|r|s|t|u|v|w|x|y|zA|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z)%'
         AND tracking NOT LIKE '%DHL%'
         AND tracking NOT LIKE '%UPS%'
         AND tracking NOT LIKE '%CPAP%'
         AND tracking NOT LIKE 'LK%'
         AND tracking NOT LIKE '1Z4FY%'
         AND tracking NOT LIKE 'R%')
   GROUP BY order_id) s ON s.order_id = o.id
WHERE o.completed_at IS NOT NULL
  AND o.shipment_state = 'shipped'
ORDER BY order_date;

 -- \copy (SELECT o.id order_id, o.number order_number, o.completed_at::DATE order_date, s.spree_tracking_numbers, s.tracking_numbers, s.ship_date FROM spree_orders o INNER JOIN ( SELECT order_id, STRING_AGG(DISTINCT tracking, ',') tracking_numbers, STRING_AGG(DISTINCT number, ',') spree_tracking_numbers, MAX(shipped_at::DATE) ship_date FROM spree_shipments WHERE tracking IS NULL OR (tracking SIMILAR TO '%(a|b|c|d|e|f|g|h|i|j|k|l|m|n|o|p|q|r|s|t|u|v|w|x|y|zA|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z)%' AND tracking NOT LIKE '%DHL%' AND tracking NOT LIKE '%UPS%' AND tracking NOT LIKE '%CPAP%' AND tracking NOT LIKE 'LK%' AND tracking NOT LIKE '1Z4FY%' AND tracking NOT LIKE 'R%') GROUP BY order_id) s ON s.order_id = o.id WHERE o.completed_at IS NOT NULL AND o.shipment_state = 'shipped' ORDER BY order_date) to 'orders missing tracking number 2017-01-31.csv' with csv