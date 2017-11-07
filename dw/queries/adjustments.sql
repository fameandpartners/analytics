SELECT
  adjustable_id order_id,
  originator_type,
  SUM(amount) adjustments
FROM spree_adjustments
WHERE amount != 0 AND eligible AND adjustable_type = 'Spree::Order'
GROUP BY order_id, originator_type
