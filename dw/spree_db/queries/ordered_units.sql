SELECT
    o.id order_id,
    o.number order_number,
    o.state order_state,
    o.payment_state,
    (o.completed_at at time zone 'UTC') at time zone 'America/Los_Angeles' completed_timestamp,
    o.total,
    o.item_total,
    o.adjustment_total o_adjustments,
    o.email,
    o.user_id,
    o.user_first_name || ' ' || o.user_last_name customer_name,
    o.currency,
    o.ship_address_id,
    li.id line_item_id,
    li.quantity,
    li.price,
    v.product_id,
    v.height v_height,
    g.size g_size
FROM spree_orders o
INNER JOIN spree_line_items li
    ON li.order_id = o.id
LEFT JOIN spree_variants v
    ON li.variant_id = v.id
LEFT JOIN (
    SELECT DISTINCT order_id
    FROM spree_payments
    WHERE state = 'completed'
) pay ON pay.order_id = o.id
LEFT JOIN global_skus g
    ON g.sku = v.sku
WHERE completed_at IS NOT NULL;
