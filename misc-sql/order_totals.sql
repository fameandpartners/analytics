SELECT
    o.number,
    lio.li_total,
    o.item_total,
    o.adjustment_total,
    o.total,
    cust.physical_customization
FROM spree_orders o
INNER JOIN (
    SELECT order_id, SUM(price * quantity) li_total
    FROM spree_line_items
    GROUP BY order_id
) lio ON lio.order_id = o.id
LEFT JOIN (
    SELECT
        li.order_id,
        MAX(CASE WHEN lip.customization_value_ids SIMILAR TO '%([1-9])%'
            THEN 1 ELSE 0 END) physical_customization
    FROM line_item_personalizations lip
    INNER JOIN spree_line_items li
        ON li.id = lip.line_item_id
    GROUP BY li.order_id
) cust ON cust.order_id = o.id
WHERE o.completed_at IS NOT NULL
    AND o.completed_at >= '2017-01-01'

SELECT item_total - li_total delta, COUNT(*)
FROM (
    SELECT
        o.number,
        lio.li_total,
        o.item_total,
        o.adjustment_total,
        o.total,
        cust.physical_customization
    FROM spree_orders o
    INNER JOIN (
        SELECT order_id, SUM(price * quantity) li_total
        FROM spree_line_items
        GROUP BY order_id
    ) lio ON lio.order_id = o.id
    LEFT JOIN (
        SELECT
            li.order_id,
            MAX(CASE WHEN lip.customization_value_ids SIMILAR TO '%([1-9])%'
                THEN 1 ELSE 0 END) physical_customization
        FROM line_item_personalizations lip
        INNER JOIN spree_line_items li
            ON li.id = lip.line_item_id
        GROUP BY li.order_id
    ) cust ON cust.order_id = o.id
    WHERE o.completed_at IS NOT NULL
        AND o.completed_at >= '2017-01-01'
) a
WHERE physical_customization != 1
GROUP BY item_total - li_total
