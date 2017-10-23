WITH utm_orders AS
  (SELECT o.id order_id,
               MIN(m_user.created_at) m_user_min,
                                      MAX(m_order.created_at) m_order_max,
                                                              EXTRACT(EPOCH
                                                                      FROM MAX(m_order.created_at) - MIN(m_user.created_at))/60/60/24 days_to_sale
   FROM spree_orders o
   INNER JOIN marketing_order_traffic_parameters m_order ON o.id = m_order.order_id
   LEFT JOIN marketing_user_visits m_user ON o.user_id = m_user.spree_user_id
   WHERE o.completed_at IS NOT NULL
   GROUP BY o.id)
SELECT AVG(days_to_sale)
FROM utm_orders
SELECT utm1.order_id,
       utm1.utm_source,
       utm1.created_at::DATE
FROM marketing_order_traffic_parameters utm1
INNER JOIN marketing_order_traffic_parameters utm2 ON (utm1.order_id = utm2.order_id
                                                       AND utm1.created_at < utm2.created_at)
FROM spree_orders o
INNER JOIN marketing_order_traffic_parameters utm ON o.id = utm.order_id
LEFT JOIN marketing_user_visits utm2 ON o.user_id = utm2.spree_user_id
WHERE o.completed_at IS NOT NULL -- monthly revenue last touch attribution by utm source

  SELECT EXTRACT(YEAR
                 FROM motp.last_touch_date) last_touch_year,
         EXTRACT(MONTH
                 FROM motp.last_touch_date) last_touch_month,
         motp.utm_source,
         SUM(o.total) revenue,
         COUNT(*) orders
  FROM spree_orders o
  INNER JOIN
    ( SELECT tp1.order_id,
             STRING_AGG(DISTINCT tp1.utm_source, ',') utm_source,
             MAX(tp2.last_touch_date) last_touch_date
     FROM marketing_order_traffic_parameters tp1
     INNER JOIN
       ( SELECT order_id,
                MAX(created_at) last_touch_date
        FROM marketing_order_traffic_parameters
        WHERE utm_source SIMILAR TO '%([a-zA-Z])%'
        GROUP BY order_id) tp2 ON tp1.order_id = tp2.order_id
     AND tp1.created_at = tp2.last_touch_date
     GROUP BY tp1.order_id) motp ON motp.order_id = o.id WHERE o.completed_at IS NOT NULL
  AND o.state = 'complete'
  AND o.payment_state = 'paid'
  AND o.total > 0
GROUP BY EXTRACT(YEAR
                 FROM motp.last_touch_date),
         EXTRACT(MONTH
                 FROM motp.last_touch_date),
         motp.utm_source
ORDER BY last_touch_year,
         last_touch_month,
         revenue DESC -- SELECT   EXTRACT(YEAR FROM motp.last_touch_date) last_touch_year,  EXTRACT(MONTH FROM motp.last_touch_date) last_touch_month,  motp.utm_source,  SUM(o.total) revenue,  COUNT(*) orders  FROM spree_orders o  INNER JOIN (  SELECT   tp1.order_id,   STRING_AGG(DISTINCT tp1.utm_source, ',') utm_source,   MAX(tp2.last_touch_date) last_touch_date  FROM marketing_order_traffic_parameters tp1   INNER JOIN (  SELECT order_id, MAX(created_at) last_touch_date  FROM marketing_order_traffic_parameters  WHERE utm_source SIMILAR TO '%([a-zA-Z])%'  GROUP BY order_id) tp2  ON tp1.order_id = tp2.order_id AND tp1.created_at = tp2.last_touch_date  GROUP BY tp1.order_id) motp  ON motp.order_id = o.id  WHERE o.completed_at IS NOT NULL  AND o.state = 'complete'  AND o.payment_state = 'paid'  AND o.total > 0  GROUP BY   EXTRACT(YEAR FROM motp.last_touch_date),  EXTRACT(MONTH FROM motp.last_touch_date),  motp.utm_source  ORDER BY last_touch_year, last_touch_month, revenue DESC
 -- monthly revenue first touch attribution by utm source

SELECT EXTRACT(YEAR
               FROM muv.first_touch_date) first_touch_year,
       EXTRACT(MONTH
               FROM muv.first_touch_date) first_touch_month,
       muv.utm_source,
       SUM(o.total) revenue,
       COUNT(*) orders
FROM spree_orders o
INNER JOIN
  ( SELECT muv1.spree_user_id,
           MAX(muv1.utm_source) utm_source,
           MAX(muv2.first_touch_date) first_touch_date
   FROM marketing_user_visits muv1
   INNER JOIN
     ( SELECT spree_user_id,
              MIN(created_at) first_touch_date
      FROM marketing_user_visits
      WHERE utm_source SIMILAR TO '%([a-zA-Z])%'
      GROUP BY spree_user_id) muv2 ON muv1.spree_user_id = muv2.spree_user_id
   AND muv1.created_at = muv2.first_touch_date
   GROUP BY muv1.spree_user_id) muv ON muv.spree_user_id = o.user_id
WHERE o.completed_at IS NOT NULL
  AND o.state = 'complete'
  AND o.payment_state = 'paid'
  AND o.total > 0
  AND o.completed_at::DATE > muv.first_touch_date::DATE
GROUP BY EXTRACT(YEAR
                 FROM muv.first_touch_date),
         EXTRACT(MONTH
                 FROM muv.first_touch_date),
         muv.utm_source
ORDER BY first_touch_year,
         first_touch_month,
         revenue DESC -- SELECT  EXTRACT(YEAR FROM muv.first_touch_date) first_touch_year,  EXTRACT(MONTH FROM muv.first_touch_date) first_touch_month,  muv.utm_source,  SUM(o.total) revenue,  COUNT(*) orders  FROM spree_orders o  INNER JOIN (  SELECT   muv1.spree_user_id,   MAX(muv1.utm_source) utm_source,   MAX(muv2.first_touch_date) first_touch_date  FROM marketing_user_visits muv1   INNER JOIN (  SELECT   spree_user_id,   MIN(created_at) first_touch_date  FROM marketing_user_visits  WHERE utm_source SIMILAR TO '%([a-zA-Z])%'  GROUP BY spree_user_id) muv2  ON muv1.spree_user_id = muv2.spree_user_id AND muv1.created_at = muv2.first_touch_date  GROUP BY muv1.spree_user_id) muv  ON muv.spree_user_id = o.user_id  WHERE o.completed_at IS NOT NULL  AND o.state = 'complete'  AND o.payment_state = 'paid'  AND o.total > 0 AND o.completed_at::DATE > muv.first_touch_date::DATE GROUP BY   EXTRACT(YEAR FROM muv.first_touch_date),  EXTRACT(MONTH FROM muv.first_touch_date),  muv.utm_source  ORDER BY first_touch_year, first_touch_month, revenue DESC

SELECT EXTRACT(YEAR
               FROM muv.first_touch_date) first_touch_year,
       EXTRACT(MONTH
               FROM muv.first_touch_date) first_touch_month,
       muv.utm_source,
       SUM(o.total) revenue,
       COUNT(*) orders,
       AVG(o.completed_at::DATE - muv.first_touch_date::DATE) avg_days_to_sale
FROM spree_orders o
INNER JOIN
  ( SELECT muv1.spree_user_id,
           MAX(muv1.utm_source) utm_source,
           MAX(muv2.first_touch_date) first_touch_date
   FROM marketing_user_visits muv1
   INNER JOIN
     ( SELECT spree_user_id,
              MIN(created_at) first_touch_date
      FROM marketing_user_visits
      WHERE utm_source SIMILAR TO '%([a-zA-Z])%'
      GROUP BY spree_user_id) muv2 ON muv1.spree_user_id = muv2.spree_user_id
   AND muv1.created_at = muv2.first_touch_date
   GROUP BY muv1.spree_user_id) muv ON muv.spree_user_id = o.user_id
WHERE o.completed_at IS NOT NULL
  AND o.state = 'complete'
  AND o.payment_state = 'paid'
  AND o.total > 0
  AND o.completed_at::DATE > muv.first_touch_date::DATE
GROUP BY EXTRACT(YEAR
                 FROM muv.first_touch_date),
         EXTRACT(MONTH
                 FROM muv.first_touch_date),
         muv.utm_source
ORDER BY first_touch_year,
         first_touch_month,
         revenue DESC