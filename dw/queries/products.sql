SELECT 
    p.id product_id, 
    g.style style_number, 
    INITCAP(p.name) style_name, 
    INITCAP(f.name) factory_name,
    p.hidden, 
    p.deleted_at, 
    p.available_on 
FROM spree_products p 
LEFT JOIN ( 
    SELECT 
        product_id, 
        STRING_AGG(DISTINCT UPPER(sku), ',') style 
    FROM spree_variants 
    WHERE is_master 
    GROUP BY product_id 
) g ON g.product_id = p.id
LEFT JOIN factories f 
    ON p.factory_id = f.id
