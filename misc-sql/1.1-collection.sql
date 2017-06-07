SELECT DISTINCT product_id, UPPER(style_number) style_number, product_name
FROM global_skus
WHERE UPPER(style_number) IN ('FP2250','FP2251','FP2252','FP2253','FP2254','FP2254B','FP2255','FP2255P','FP2256','FP2257','FP2258','FP2259','FP2260','FP2261','FP2261P','FP2262','FP2263','FP2272','FP2273','FP2273M','FP2274','FP2275','FP2276','FP2278','FP2280','FP2281','FP2282','FP2283','FP2284','FP2286','FP2287','FP2287B','FP2287P','FP2288','FP2289','FP2290','FP2293','FP2294','FP2295','FP2296B','FP2297','FP2298','FP2298P','FP2299','FP2300','FP2304','FP2307','FP2308','FP2320','FP2321','FP2322','FP2323','FP2324','FP2359','FPMC214','FPMC230','FPMC264','FPMC266')

-- SELECT product_id, UPPER(style_number) style_number, product_name FROM global_skus WHERE UPPER(style_number) IN ('FP2250','FP2251','FP2252','FP2253','FP2254','FP2254B','FP2255','FP2255P','FP2256','FP2257','FP2258','FP2259','FP2260','FP2261','FP2261P','FP2262','FP2263','FP2272','FP2273','FP2273M','FP2274','FP2275','FP2276','FP2278','FP2280','FP2281','FP2282','FP2283','FP2284','FP2286','FP2287','FP2287B','FP2287P','FP2288','FP2289','FP2290','FP2293','FP2294','FP2295','FP2296B','FP2297','FP2298','FP2298P','FP2299','FP2300','FP2304','FP2307','FP2308','FP2320','FP2321','FP2322','FP2323','FP2324','FP2359','FPMC214','FPMC230','FPMC264','FPMC266')

