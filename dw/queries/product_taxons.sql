SELECT pt.product_id, t.name taxon_name
FROM spree_products_taxons pt
JOIN spree_taxons t on t.id = pt.taxon_id
