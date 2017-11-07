SELECT sa.id ship_address_id,
       INITCAP(sa.city) ship_city,
       INITCAP(COALESCE(ss.name, sa.state_name)) ship_state,
       INITCAP(sc.name) ship_country
FROM spree_addresses sa
LEFT JOIN spree_states ss ON ss.id = sa.state_id
LEFT JOIN spree_countries sc ON sc.id = sa.country_id