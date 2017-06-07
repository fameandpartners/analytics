SELECT completed_at::DATE order_date, SUM(total)
FROM spree_orders
WHERE completed_at IS NOT NULL
    AND payment_state = 'paid'
    AND total > 0
GROUP BY completed_at::DATE
ORDER BY completed_at::DATE DESC
