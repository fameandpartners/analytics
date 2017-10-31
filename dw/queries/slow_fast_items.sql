SELECT
  limo.line_item_id,
  pmo.option_type making_option
FROM line_item_making_options limo
INNER JOIN product_making_options pmo ON pmo.id = limo.making_option_id
