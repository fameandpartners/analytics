SELECT lip.line_item_id,
       MAX(CASE WHEN lip.customization_value_ids SIMILAR TO '%(\[0-9\])%' THEN 1 ELSE 0 END) customized,
       STRING_AGG(DISTINCT lip.size, ', ') lip_size,
       STRING_AGG(DISTINCT lip.customization_value_ids, '/n') customisation_value_ids,
       INITCAP(STRING_AGG(DISTINCT lip.color, ', ')) color,
       INITCAP(STRING_AGG(DISTINCT lip.height, ', ')) lip_height
FROM line_item_personalizations lip
GROUP BY line_item_id;