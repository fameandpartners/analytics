SELECT
  s.order_id,
  liu.line_item_id ship_line_item_id,
  s.shipped_at
FROM spree_shipments s
LEFT JOIN (
  SELECT DISTINCT
    line_item_id,
    shipment_id
  FROM line_item_updates
  WHERE match_errors not like '%:%'
    AND shipment_errors not similar to '%([a-zA-Z])%'
    AND line_item_id is not null
    AND shipment_id is not null
  ) liu ON liu.line_item_id = s.id
WHERE s.shipped_at IS NOT NULL
