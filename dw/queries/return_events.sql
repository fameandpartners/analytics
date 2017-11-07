SELECT
  item_return_uuid,
  STRING_AGG(DISTINCT TRIM(SPLIT_PART(SPLIT_PART(ire.data, '@', 1), 'user:', 2)), ' and ') return_processed_by
FROM item_return_events ire
WHERE ire.event_type = 'receive_item'
GROUP BY item_return_uuid
