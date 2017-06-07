-- wedding app performance - launched Jan 30 2017

-- Visits
-- Traffic source
-- %/# of visits that result in sign-up or entry into app
-- # of boards
-- # of people on the boards

-- Purchases ($ and #)
-- What they buy
-- Customizations
-- Avg. number of bridesmaids in a party
-- Is it the bride or bridesmaid making the board
-- Number of uses before purchase
-- Where do people fall out of the experience?
-- Also things like
-- 1. What dress customisations are being added to the boards
-- 2. What dress customisations are most popular
-- 3. Sign up funnel with drop off
-- 4. Checkout Funnel 
-- 5. Also product sales based on customisations/SKU’s
-- 6. Are they brides/bridesmaids inviting others to the board and how often

-- Base Silhouette product ids: 1316,1317,1318,1319,1320,1321,1322,1323

-- Orders from wedding app by day
SELECT 
	completed_at::DATE order_date, 
	COUNT(*) items_sold, 
	SUM(li.price * li.quantity * (CASE WHEN li.currency = 'AUD' THEN 0.75 ELSE 1 END)) revenue,
	COUNT(DISTINCT w.user_id) customers
FROM spree_line_items li
INNER JOIN spree_orders o
	ON o.id = li.order_id
INNER JOIN spree_variants v
	ON v.id = li.variant_id
INNER JOIN spree_products p
	ON p.id = v.product_id
INNER JOIN (
	-- wedding app users
	SELECT DISTINCT user_id
	FROM wedding_atelier_users_event_roles) w
	ON w.user_id = o.user_id
WHERE completed_at IS NOT NULL
	AND p.id IN (1316,1317,1318,1319,1320,1321,1322,1323)
	AND o.email NOT LIKE '%fameandpartners.com'
	AND o.email != 'gradient@gmail.com'
GROUP BY order_date
ORDER BY order_date

-- Orders from wedding app
SELECT 
	COUNT(DISTINCT w.user_id) customers,
	COUNT(*) items_sold, 
	SUM(li.price * li.quantity * (CASE WHEN li.currency = 'AUD' THEN 0.75 ELSE 1 END)) sub_total
FROM spree_line_items li
INNER JOIN spree_orders o
	ON o.id = li.order_id
INNER JOIN spree_variants v
	ON v.id = li.variant_id
INNER JOIN spree_products p
	ON p.id = v.product_id
INNER JOIN (
	-- wedding app users
	SELECT DISTINCT user_id
	FROM wedding_atelier_users_event_roles) w
	ON w.user_id = o.user_id
WHERE completed_at IS NOT NULL
	AND p.id IN (1316,1317,1318,1319,1320,1321,1322,1323)
	AND o.email NOT LIKE '%fameandpartners.com'
	AND o.email != 'gradient@gmail.com'
	AND o.total >= 0

-- boards
SELECT owner_id board_owner, created_at::DATE board_create_date
FROM wedding_atelier_events



-- Boards and people on boards by day
SELECT 
	board.created_at::DATE board_ceated_date,
	COUNT(DISTINCT invite.event_id) boards_created, 
	COUNT(DISTINCT invite.user_email) + 1 people_on_the_boards,
	COUNT(DISTINCT CASE WHEN roles.name IN ('bridesmaid','maid of honor') THEN invite.user_email END) bridesmaids_on_the_boards,
	COUNT(DISTINCT dresses.id) dressed_added
FROM wedding_atelier_invitations invite
LEFT JOIN spree_users u
	ON u.email = invite.user_email
LEFT JOIN wedding_atelier_events board
	ON invite.event_id = board.id
LEFT JOIN wedding_atelier_users_event_roles uer
	ON uer.user_id = u.id
LEFT JOIN wedding_atelier_event_roles roles
	ON roles.id = uer.event_role_id
LEFT JOIN wedding_atelier_event_dresses dresses
	ON dresses.event_id = board.id
WHERE invite.state = 'accepted'
	AND u.email NOT LIKE '%fameandpartners.com'
	AND u.email != 'gradient@gmail.com'
GROUP BY board.created_at::DATE
ORDER BY board.created_at::DATE

-- \copy (SELECT  board.created_at::DATE board_ceated_date, COUNT(DISTINCT invite.event_id) boards_created,  COUNT(DISTINCT invite.user_email) + 1 people_on_the_boards, COUNT(DISTINCT CASE WHEN roles.name IN ('bridesmaid','maid of honor') THEN invite.user_email END) bridesmaids_on_the_boards FROM wedding_atelier_invitations invite INNER JOIN spree_users u ON u.email = invite.user_email LEFT JOIN wedding_atelier_events board ON invite.event_id = board.id LEFT JOIN wedding_atelier_users_event_roles uer ON uer.user_id = u.id LEFT JOIN wedding_atelier_event_roles roles ON roles.id = uer.event_role_id WHERE invite.state = 'accepted' GROUP BY board.created_at::DATE ORDER BY board.created_at::DATE) to 'wedding boards and people by day 2017-02-21.csv' with csv

-- Sign ups by day
SELECT 
	er.signup_date,
	COUNT(DISTINCT uer.user_id) signups
FROM wedding_atelier_users_event_roles uer
INNER JOIN (
	SELECT id, MIN(created_at::DATE) signup_date
	FROM wedding_atelier_event_roles
	GROUP BY id) er
	ON er.id = uer.event_role_id
GROUP BY er.signup_date
ORDER BY er.signup_date

# Wedding App Customers
SELECT 
	DISITNCT w.user_id converted_wedding_user_id
FROM spree_line_items li
INNER JOIN spree_orders o
	ON o.id = li.order_id
INNER JOIN spree_variants v
	ON v.id = li.variant_id
INNER JOIN spree_products p
	ON p.id = v.product_id
INNER JOIN (
	-- wedding app users
	SELECT DISTINCT user_id
	FROM wedding_atelier_users_event_roles) w
	ON w.user_id = o.user_id
WHERE completed_at IS NOT NULL
	AND p.id IN (1316,1317,1318,1319,1320,1321,1322,1323)
	AND o.email NOT LIKE '%fameandpartners.com'
	AND o.email != 'gradient@gmail.com'

-- \copy (SELECT  er.signup_date, COUNT(DISTINCT uer.user_id) signups FROM wedding_atelier_users_event_roles uer INNER JOIN ( SELECT id, MAX(created_at::DATE) signup_date FROM wedding_atelier_event_roles GROUP BY id) er ON er.id = uer.event_role_id GROUP BY er.signup_date ORDER BY er.signup_date) to 'wedding app signups by day 2017-02-21.csv' with csv

-- total signups
SELECT 
	COUNT(DISTINCT uer.user_id) signups
FROM wedding_atelier_users_event_roles uer
INNER JOIN (
	SELECT id, MIN(created_at::DATE) signup_date
	FROM wedding_atelier_event_roles
	GROUP BY id) er
	ON er.id = uer.event_role_id

SELECT 
	er.first_wedding_event,
	u.created_at user_created_at,
	u.email
FROM wedding_atelier_users_event_roles uer
INNER JOIN (
	SELECT id, MAX(created_at) first_wedding_event
	FROM wedding_atelier_event_roles
	GROUP BY id) er
	ON er.id = uer.event_role_id
INNER JOIN spree_users u 
	ON u.id = uer.user_id
ORDER BY er.first_wedding_event

-- SELECT DISTINCT  er.signup_date, u.email FROM wedding_atelier_users_event_roles uer INNER JOIN ( SELECT id, MAX(created_at::DATE) signup_date FROM wedding_atelier_event_roles GROUP BY id) er ON er.id = uer.event_role_id INNER JOIN spree_users u  ON u.id = uer.user_id ORDER BY er.signup_date

select inv.user_email, e.created_at::date
from wedding_atelier_invitations inv
join wedding_atelier_events e
	ON inv.event_id = e.id
WHERE inv.state = 'accepted'
ORDER BY e.created_at::date


SELECT 
	invite.state,
	roles.name,
	COUNT(DISTINCT u.email)
FROM wedding_atelier_invitations invite
INNER JOIN spree_users u
	ON u.email = invite.user_email
LEFT JOIN wedding_atelier_events board
	ON invite.event_id = board.id
LEFT JOIN wedding_atelier_users_event_roles uer
	ON uer.user_id = u.id
LEFT JOIN wedding_atelier_event_roles roles
	ON roles.id = uer.event_role_id
LEFT JOIN wedding_atelier_event_dresses dresses
	ON dresses.event_id = board.id
WHERE u.email NOT LIKE '%fameandpartners.com'
	AND u.email != 'gradient@gmail.com'
	AND roles.name IS NOT NULL
GROUP BY
	invite.state,
	roles.name


SELECT u.id IS NOT NULL fandp_user, COUNT(DISTINCT invite.user_email), COUNT(*)
FROM wedding_atelier_invitations invite
LEFT JOIN spree_users u
	ON u.email = invite.user_email
GROUP BY u.id IS NOT NULL

-- users created during the 2/20 spike

SELECT 
	EXTRACT(HOUR FROM first_wedding_event) signup_hour,
	COUNT(DISTINCT email) users
FROM (
	SELECT 
		er.first_wedding_event,
		u.created_at user_created_at,
		u.email
	FROM wedding_atelier_users_event_roles uer
	INNER JOIN (
		SELECT id, MAX(created_at) first_wedding_event
		FROM wedding_atelier_event_roles
		GROUP BY id) er
		ON er.id = uer.event_role_id
	INNER JOIN spree_users u 
		ON u.id = uer.user_id
	WHERE er.first_wedding_event::DATE = '2017-02-20'
	ORDER BY er.first_wedding_event) spike
GROUP BY EXTRACT(HOUR FROM first_wedding_event)


SELECT 
	COUNT(DISTINCT users.user_id)
FROM wedding_atelier_events board
LEFT JOIN wedding_atelier_event_roles roles
	ON roles.resource_id = board.id
LEFT JOIN wedding_atelier_users_event_roles users
	ON users.event_role_id = roles.id
WHERE board.name IS NOT NULL

SELECT 
	created_at::DATE created_date,
	COUNT(*)
FROM wedding_atelier_events board
WHERE board.name IS NOT NULL
GROUP BY created_at::DATE
ORDER BY created_at::DATE

SELECT e.created_at::DATE signup_date, 
	COUNT(DISTINCT e.owner_id) brides,
	COUNT(DISTINCT d.id) dresses_added
FROM wedding_atelier_events e
LEFT JOIN wedding_atelier_event_dresses d
	ON d.event_id = e.id
INNER JOIN spree_users u
	ON u.id = e.owner_id
GROUP BY e.created_at::DATE

-- SELECT e.created_at::DATE signup_date,  COUNT(DISTINCT e.owner_id) brides, COUNT(DISTINCT d.id) dresses_added FROM wedding_atelier_events e LEFT JOIN wedding_atelier_event_dresses d ON d.event_id = e.id INNER JOIN spree_users u ON u.id = e.owner_id GROUP BY e.created_at::DATE