SELECT email, min(completed_at::DATE) first_order_date
FROM spree_orders
WHERE completed_at IS NOT NULL
    AND email IS NOT NULL
GROUP BY email;