-- Contemporary Customer: any customer that purchases a dress with any of these taxons:
-- skirt, cotton, slip, little white dress, little black dress, ruffle
-- SELECT * from table WHERE column SIMILAR TO '(AAA|BBB|CCC)%';

-- contemporary taxons
SELECT name
FROM spree_taxons
WHERE LOWER(name) SIMILAR TO '%(skirt|cotton|slip|little white dress|little black dress|ruffle)%';

-- "Contemporary" products based on taxon
SELECT DISTINCT p.id product_id
FROM spree_products_taxons pt
JOIN spree_taxons t
	ON t.id = pt.taxon_id
JOIN spree_products p
	ON p.id = pt.product_id
WHERE LOWER(t.name) SIMILAR TO '%(skirt|cotton|slip|little white dress|little black dress|ruffle)%';

-- Products with Taxons
SELECT p.id product_id, t.name taxon
FROM spree_products_taxons pt
JOIN spree_taxons t
	ON t.id = pt.taxon_id
JOIN spree_products p
	ON p.id = pt.product_id

-- All Categories
SELECT  
	p.id product_id,
	STRING_AGG(DISTINCT CASE 
		WHEN LOWER(t.name) SIMILAR TO '(mini|knee|petti)' THEN 'Short'
		WHEN LOWER(t.name) SIMILAR TO '(midi|ankle|maxi)' THEN 'Long'
	END, ',') short_long,
	MAX(CASE WHEN LOWER(t.name) = 'skirt' OR LOWER(p.name) LIKE '%skirt%' THEN 1 ELSE 0 END) skirt,
	MAX(CASE WHEN t.name = 'Evening' THEN 1 ELSE 0 END) evening,
	MAX(CASE WHEN t.name = 'Cocktail'THEN 1 ELSE 0 END) cocktail,
	MAX(CASE WHEN LOWER(t.name) like '%brid%' THEN 1 ELSE 0 END) bridal,
	MAX(CASE WHEN t.name = 'Daytime' THEN 1 ELSE 0 END) daytime,
	MAX(CASE WHEN t.name SIMILAR TO '%(skirt|cotton|slip|little white dress|little black dress|ruffle)%') contemporary
FROM spree_products_taxons pt
JOIN spree_taxons t
	ON t.id = pt.taxon_id
JOIN spree_products p
	ON p.id = pt.product_id
GROUP BY p.id

SELECT DISTINCT p.id product_id, p.name product_name
FROM spree_products_taxons pt
JOIN spree_taxons t
	ON t.id = pt.taxon_id
JOIN spree_products p
	ON p.id = pt.product_id
JOIN spree_variants v
	ON p.id = v.product_id
WHERE t.name = 'Base Silhouette'
--  product_id
-- ------------
--        1320
--        1317
--        1322
--        1323
--        1321
--        1319
--        1318
--        1316

-- products with or without length
SELECT 
	sum(case when lower(taxons) similar to '%(maxi|midi|mini|knee|petti|ankle)%' 
		then 1 else 0 end) length_labeled,
	sum(1) no_length
FROM (
	SELECT p.id product_id, STRING_AGG(DISTINCT t.name, ' , ') taxons
	FROM spree_products_taxons pt
	JOIN spree_taxons t
		ON t.id = pt.taxon_id
	JOIN spree_products p
		ON p.id = pt.product_id
	GROUP BY p.id) t
--  length_labeled | no_length
-- ----------------+-----------
--             497 |      1109