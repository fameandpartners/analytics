select 
	utm_medium, 
	COUNT(DISTINCT utm_source || utm_medium || utm_campaign || order_id) 
from marketing_order_traffic_parameters 
where utm_source is not null  
	AND created_at >= current_date - 7
group by utm_medium 
order by COUNT(DISTINCT utm_source || utm_medium || utm_campaign || order_id) desc
LIMIT 10;

SELECT 
	o_utm.max_updated_at::DATE touch_date,
	o.state, 
	o_utm.utm_source,
	COUNT(*) orders,
	SUM(o.total * CASE WHEN o.currency = 'AUD' THEN 0.75 ELSE 1 END) revenue_usd
FROM ( -- Most Recent UTM Tags by order_id
	SELECT *,
		MAX(updated_at) OVER (PARTITION BY order_id) max_updated_at
	FROM marketing_order_traffic_parameters
	WHERE updated_at < CURRENT_DATE) o_utm
INNER JOIN spree_orders o
	ON o.id = o_utm.order_id
INNER JOIN ( -- Limit to legit cart adds but keep at order level
	SELECT DISTINCT order_id
	FROM spree_line_items
	WHERE updated_at < CURRENT_DATE) li
	ON li.order_id = o.id
WHERE o_utm.utm_source IS NOT NULL
	AND o_utm.updated_at = o_utm.max_updated_at
	AND o_utm.max_updated_at < CURRENT_DATE
	AND o.updated_at < CURRENT_DATE
GROUP BY 
	o_utm.max_updated_at::DATE,
	o.state, 
	o_utm.utm_source

-- \copy (SELECT  o_utm.max_updated_at::DATE touch_date, o.state,  o_utm.utm_source, COUNT(DISTINCT o_utm.order_id) orders, SUM(o.total * CASE WHEN o.currency = 'AUD' THEN 0.75 ELSE 1 END) revenue_usd FROM (  SELECT *, MAX(updated_at) OVER (PARTITION BY order_id) max_updated_at FROM marketing_order_traffic_parameters WHERE updated_at < CURRENT_DATE - 1 ) o_utm INNER JOIN spree_orders o ON o.id = o_utm.order_id INNER JOIN (  SELECT DISTINCT order_id FROM spree_line_items WHERE updated_at < CURRENT_DATE - 1) li ON li.order_id = o.id WHERE o_utm.utm_source IS NOT NULL AND o_utm.updated_at = o_utm.max_updated_at AND o_utm.max_updated_at < CURRENT_DATE - 1 AND o.updated_at < CURRENT_DATE - 1 GROUP BY  o_utm.max_updated_at::DATE, o.state,  o_utm.utm_source) to 'utm and order.csv' with csv