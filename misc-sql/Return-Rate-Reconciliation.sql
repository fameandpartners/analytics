-- Payment Lookup

SELECT DISTINCT *
FROM
  (SELECT order_id,
          response_code
   FROM spree_payments
   WHERE response_code IS NOT NULL
     AND source_type = 'Spree::CreditCard'
   UNION SELECT p.order_id,
                pp.transaction_id
   FROM spree_payments p
   INNER JOIN spree_paypal_express_checkouts pp ON p.source_id = pp.id
   WHERE pp.transaction_id IS NOT NULL
     AND p.source_type = 'Spree::PaypalExpressCheckout'
   UNION SELECT li.order_id,
                ir.refund_ref
   FROM item_returns ir
   INNER JOIN spree_line_items li ON ir.line_item_id = li.id
   WHERE ir.refund_ref IS NOT NULL
   UNION SELECT order_id,
                refund_ref
   FROM refund_requests
   WHERE refund_ref IS NOT NULL) -- Order Returns

SELECT li.order_id,
       STRING_AGG(DISTINCT o.number, ',') order_number,
       STRING_AGG(DISTINCT o.state, ',') order_status,
       MAX(s.shipped_at::DATE) ship_date,
       COUNT(DISTINCT li.id) items,
       SUM(CASE WHEN li.currency = 'AUD' THEN li.price * li.quantity * 0.75 ELSE li.price * li.quantity END) original_amount_usd,
       SUM(CASE WHEN ir.id IS NOT NULL THEN CASE WHEN ir.order_paid_currency = 'AUD' THEN ir.refund_amount * 0.75 ELSE ir.refund_amount END ELSE CASE WHEN rr.refund_currency = 'AUD' THEN rr.refund_amount * 0.75 ELSE rr.refund_amount END END) / 100 refund_amount_usd,
       STRING_AGG(DISTINCT ir.requested_action, ',') requested_action,
       STRING_AGG(DISTINCT CASE WHEN ir.id IS NOT NULL THEN ir.acceptance_status ELSE rr.acceptance_status END, ',') acceptance_status
FROM spree_line_items li
INNER JOIN spree_orders o ON o.id = li.order_id
LEFT JOIN item_returns ir ON ir.line_item_id = li.id
LEFT JOIN refund_requests rr ON rr.order_id = li.order_id
LEFT JOIN spree_shipments s ON s.order_id = o.id
WHERE (ir.id IS NOT NULL
       OR rr.id IS NOT NULL)
  AND o.completed_at IS NOT NULL
GROUP BY li.order_id -- rev0 dirty code
-- pin

SELECT p.id,
       p.order_id,
       p.amount,
       p.response_code,
       s.ship_date,
       o.number order_number,
       s.tracking_number,
       o.state order_state
FROM spree_payments p
INNER JOIN spree_orders o ON o.id = p.order_id
LEFT JOIN
  ( SELECT order_id,
           MAX(shipped_at::DATE) ship_date
   FROM spree_shipments
   GROUP BY order_id) s ON s.order_id = o.id
WHERE p.source_type = 'Spree::CreditCard'
  AND o.completed_at IS NOT NULL
  SELECT p.id,
         p.order_id,
         o.number order_number,
         o.state order_state,
         pp.token,
         pp.transaction_id
  FROM spree_payments p
  INNER JOIN spree_orders o ON o.id = p.order_id
  INNER JOIN spree_paypal_express_checkouts pp ON p.source_id = pp.id WHERE p.source_type = 'Spree::PaypalExpressCheckout'
  AND o.completed_at IS NOT NULL
  AND pp.transaction_id IS NOT NULL
  SELECT ir.*,
         li.order_id,
         s.ship_date ship_date_r,
         s.tracking_number_r
  FROM item_returns ir
  INNER JOIN spree_line_items li ON li.id = ir.line_item_id
  LEFT JOIN
    ( SELECT order_id,
             MAX(tracking) tracking_number_r,
             MAX(shipped_at::DATE) ship_date
     FROM spree_shipments
     GROUP BY order_id) s ON s.order_id = li.order_id
  SELECT order_id,
         order_number,
         refund_ref,
         refund_amount
  FROM refund_requests