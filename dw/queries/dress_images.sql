SELECT
  p.id product_id,
  a.id asset_id,
  a.attachment_file_name,
  a.attachment_width,
  a.attachment_height
FROM spree_assets a
INNER JOIN product_color_values pcv ON a.viewable_id = pcv.id
INNER JOIN spree_products p ON p.id = pcv.product_id
WHERE a.attachment_width < 2000
  AND a.viewable_type = 'ProductColorValue'
  AND a.attachment_file_name ilike '%front-crop%'
