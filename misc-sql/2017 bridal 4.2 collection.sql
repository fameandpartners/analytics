SELECT DISTINCT product_id,
                UPPER(style_number) style_number,
                product_name
FROM global_skus
WHERE UPPER(style_number) IN ('FP2212',
                              'FP2213',
                              'FP2214',
                              'FP2215',
                              'FP2216',
                              'FP2220',
                              'FP2218',
                              'FP2219') -- SELECT DISTINCT product_id, UPPER(style_number) style_number, product_name FROM global_skus WHERE UPPER(style_number) IN ('FP2212','FP2213','FP2214','FP2215','FP2216','FP2220','FP2218','FP2219')