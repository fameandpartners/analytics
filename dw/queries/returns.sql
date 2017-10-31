SELECT 
    "requested_at" AS "requested_at", 
    "refunded_at" AS "refunded_at", 
    "line_item_id" AS "line_item_id", 
    "refund_amount" AS "refund_amount", 
    "comments" AS "return_comments", 
    "reason_category" AS "reason_category", 
    "reason_sub_category" AS "reason_sub_category", 
    "acceptance_status" AS "acceptance_status", 
    "factory_fault" AS "factory_fault", 
    "factory_fault_reason" AS "factory_fault_reason", 
    "uuid" AS "item_return_uuid"
FROM "item_returns";
