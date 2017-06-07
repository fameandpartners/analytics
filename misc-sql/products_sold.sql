SELECT
    li.id line_item_id,
    li.order_id,
    o.number order_number,
    o.state order_state,
    o.payment_state,
    li.quantity,
    li.price,
    (
        (
     (o.total - (SUM(li.price) OVER (PARTITION BY li.order_id))
        ) / (COUNT(*) OVER (PARTITION BY li.order_id))) + li.price
    ) * CASE WHEN o.currency = 'AUD' THEN 0.77 ELSE 1 END sales_usd,
    (
        (
     (o.item_total - (SUM(li.price) OVER (PARTITION BY li.order_id)
        )
    ) / (COUNT(*) OVER (PARTITION BY li.order_id))) + li.price
    ) * CASE WHEN o.currency = 'AUD' THEN 0.77 ELSE 1 END gross_revenue_usd,
    o.adjustment_total / (COUNT(*) OVER (PARTITION BY li.order_id)
    ) * CASE WHEN o.currency = 'AUD' THEN 0.77 ELSE 1 END adjustments_usd,
    li.currency,
    loc.ship_city,
    loc.ship_state,
    loc.ship_country,
    ((o.completed_at AT TIME ZONE 'UTC') AT TIME ZONE 'America/Los_Angeles')::DATE order_date,
    COALESCE(s.ship_date, o.completed_at::DATE + 10) ship_date,
    COALESCE(s.ship_date <= CURRENT_DATE, FALSE) is_shipped,
     o.email,
     o.user_id,
     INITCAP(o.user_first_name) || ' ' || INITCAP(o.user_last_name) customer_name,
     p.id product_id,
     INITCAP(p.name) style_name,
     UPPER(style.number) style_number,
     ir.refund_amount IS NOT NULL item_returned,
     ir.refund_amount / 100 * CASE
    WHEN o.currency = 'AUD' THEN 0.77
    ELSE 1
       END refund_amount_usd,
       CASE
    WHEN o.state = 'canceled' THEN 'Canceled'
    WHEN ir.refund_amount IS NOT NULL THEN 'Returned'
    WHEN rri.line_item_id IS NOT NULL THEN 'Refund Requested'
    WHEN s.ship_date <= CURRENT_DATE THEN 'Shipped'
    ELSE 'Paid' END order_status,
    CASE WHEN LOWER(ir.reason_category) IN ('n/a',
       'na',
       'not specified',
       'not stated',
       'not satisfied') THEN 'No Reason'
    ELSE INITCAP(TRIM(ir.reason_category))
       END return_reason,
       ir.reason_sub_category,
       rri.return_request_action,
       CASE
    WHEN ir.id IS NOT NULL THEN li.order_id
       END return_order_id,
       COALESCE(cust.physical_customization, 0) physically_customized,
       cust.color,
       COALESCE(cust.size, gsku.size) SIZE,
        cust.height,
        RANK() OVER (PARTITION BY o.email
     ORDER BY o.completed_at) order_num,
      CASE
          WHEN NOT p.hidden
        AND (p.deleted_at IS NULL
      OR p.deleted_at > CURRENT_DATE)
        AND p.available_on <= CURRENT_DATE THEN 'Yes'
          ELSE 'No'
      END product_live,
      li.price * CASE
       WHEN o.currency = 'AUD' THEN 0.77
       ELSE 1
          END price_usd,
          s.ship_states,
          rri.line_item_id IS NOT NULL return_requested,
         rri.return_requested_at,
         ir.refunded_at,
         pay.o_lvl_payments::DECIMAL / (COUNT(*) OVER (PARTITION BY li.order_id)) payments
FROM spree_line_items li
LEFT JOIN spree_orders o ON o.id = li.order_id
LEFT JOIN
  (SELECT p.id,
   p.hidden,
   p.deleted_at,
   p.available_on,
   p.name,
   v.id variant_id,
   v.sku variant_sku
   FROM spree_variants v
   LEFT JOIN spree_products p ON p.id = v.product_id) p ON p.variant_id = li.variant_id
LEFT JOIN
  (SELECT sa.id,
   INITCAP(sa.city) ship_city,
   INITCAP(ss.name) ship_state,
   INITCAP(sc.name) ship_country
   FROM spree_addresses sa
   INNER JOIN spree_states ss ON ss.id = sa.state_id
   INNER JOIN spree_countries sc ON sc.id = sa.country_id) loc ON loc.id = o.ship_address_id
LEFT JOIN
  (SELECT id,
   refunded_at,
   line_item_id,
   refund_amount,
   reason_category,
   reason_sub_category
   FROM item_returns
   WHERE acceptance_status != 'rejected'
     AND line_item_id IS NOT NULL ) ir ON ir.line_item_id = li.id
LEFT JOIN
  (SELECT lip.line_item_id,
   MAX(CASE
    WHEN lip.customization_value_ids SIMILAR TO '%([1-9])%' THEN 1
    ELSE 0
END) physical_customization,
   STRING_AGG(DISTINCT lip.size, ', ') SIZE,
  INITCAP(STRING_AGG(DISTINCT lip.color, ', ')) color,
  INITCAP(STRING_AGG(DISTINCT lip.height, ', ')) height
   FROM line_item_personalizations lip
   GROUP BY line_item_id) cust ON cust.line_item_id = li.id
LEFT JOIN
  (SELECT order_id,
   MAX(shipped_at::DATE) ship_date,
   STRING_AGG(DISTINCT state, ',') ship_states
   FROM spree_shipments
   GROUP BY order_id) s ON s.order_id = li.order_id
LEFT JOIN
  (SELECT product_id,
   sku,
   SIZE
   FROM global_skus) gsku ON gsku.sku = p.variant_sku
LEFT JOIN
  (SELECT product_id,
   STRING_AGG(DISTINCT style_number, ',') number
   FROM global_skus
   GROUP BY product_id) style ON style.product_id = p.id
LEFT JOIN
  (SELECT line_item_id,
   STRING_AGG(DISTINCT reason_category, ', ') reason_category,
   STRING_AGG(DISTINCT reason, ', ') reason_sub_category,
   STRING_AGG(DISTINCT action, ', ') return_request_action,
   MAX(created_at) return_requested_at
   FROM return_request_items
   WHERE action != 'keep'
   GROUP BY line_item_id) rri ON rri.line_item_id = li.id
INNER JOIN
  (SELECT order_id,
   COUNT(*) o_lvl_payments
   FROM spree_payments
   WHERE state = 'completed'
   GROUP BY order_id) pay ON pay.order_id = o.id
WHERE o.completed_at IS NOT NULL
  AND o.completed_at >= '2015-12-21 06:43:34'
  AND o.total > 0
