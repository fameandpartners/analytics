SELECT adjustable_id order_id, STRING_AGG(label, ' ') labels
FROM spree_adjustments
WHERE amount != 0
  AND eligible
  AND adjustable_type = 'Spree::Order'
  AND originator_type = 'Spree::PromotionAction'
GROUP BY order_id
