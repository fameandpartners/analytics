library(readr)
library(RPostgreSQL)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(scales)
library(ggplot2)
library(shiny)

source("fp_init.R")

number_format <- function(x){
    dollar(x) %>%
        str_replace_all("\\$","")
}

collections <- data_frame(
    collection_null = c("SLIP","SLIP","SLIP","SLIP","SLIP","SLIP","SLIP","SLIP","SLIP","SLIP","SLIP","SLIP","SLIP","SLIP","SLIP","SLIP","SLIP","LBD / LWD","LBD / LWD","LBD / LWD","LBD / LWD","LBD / LWD","LBD / LWD","LBD / LWD","LBD / LWD","LBD / LWD","LBD / LWD","RUFFLE","RUFFLE","RUFFLE","RUFFLE","RUFFLE","RUFFLE","RUFFLE","RUFFLE","RUFFLE","RUFFLE","RUFFLE","RUFFLE","RUFFLE","RUFFLE","COTTON","COTTON","COTTON","COTTON","COTTON","COTTON","COTTON","COTTON","COTTON","COTTON","COTTON","COTTON","COTTON","COTTON","COTTON","COTTON","COTTON","COTTON","COTTON","COTTON","2.1","2.1","2.1","2.1","2.1","2.1","2.1","2.1","2.1","2.1","2.1","2.1","2.1","2.1","2.1","2.1","2.2","2.2","2.2","2.2","2.2","2.2","2.2","2.2","2.2","2.2","2.2","2.2","2.2","2.2","2.2","2.2","2.2","2.2","2.2","2.2","2.2","2.2","2.2","2.2","2.2","2.2","2.2","2.2","2.2","2.2","2.2","3.1","3.1","3.1","3.1","3.1","3.1","3.1","3.1","3.1","3.1","3.1","3.1","3.1","3.1","3.1","3.1","3.1","3.1","3.1","3.1","3.1","3.1","3.1","3.1","3.1","3.1","3.1","3.1","3.1","3.1","3.1","3.1","3.1","3.1","3.1","3.1","3.1","3.1","3.1","3.1","3.1","3.1","3.2","3.2","3.2","3.2","3.2","3.2","3.2","3.2","3.2","3.2","3.2","3.2","3.2","3.2","3.2","3.2","3.2","3.2","3.2","3.2","3.2","3.2","3.2","3.2","3.2","3.2","4.1","4.1","4.1","4.1","4.1","4.1","4.1","4.1","4.1","4.1","4.1","4.1","4.1","4.1","4.1","4.1","4.1","4.1","4.1","4.1","4.1","4.1","4.1","4.1","4.1","4.1","4.1","4.1","4.1","4.1","4.1","4.1","4.1","4.1","4.1","4.1","4.1","4.1","4.1","4.1","4.1","4.1","4.1","4.1","4.1","4.1","1.0 - preprom 2017","1.0 - preprom 2017","1.0 - preprom 2017","1.0 - preprom 2017","1.0 - preprom 2017","1.0 - preprom 2017","1.0 - preprom 2017","1.0 - preprom 2017","1.0 - preprom 2017","1.0 - preprom 2017","1.0 - preprom 2017","1.0 - preprom 2017","1.0 - preprom 2017","1.0 - preprom 2017","BRIDAL","BRIDAL","BRIDAL","BRIDAL","BRIDAL","BRIDAL","BRIDAL","BRIDAL","BRIDAL","BRIDAL","BRIDAL","BRIDAL","BRIDAL","BRIDAL","BRIDAL","BRIDAL","BRIDAL","BRIDAL","BRIDAL","BRIDAL","BRIDAL"),
    sytle_name = c("Ainsley","Cohen","Wyatt","Sparrow","Audrey","Vance Clover","Rylan","Geo Trek","Vesper","Ziv","Lux","Kennedy-Grace","Lyndsey","Emery","Kai","Paxton","Flynn","Katrina","Cadence","Tarren","Briley","Ira","Danielle","Teresa","Maizy","Shaylyn","Oneilia","Dara Jumpsuit","Think Twice Playsuit","Addison Playsuit","Maeve Two Piece","Sasha","Koryn","Harley","Callais","Maaike","Jensen","Kelis","Payton","Aliya","Greer Two Piece","Colby","Jordyn","Emerson","Gianna","Colette","Whitney","Sevia","Liesl Two Piece","Merille Two Piece","Cenit Two Piece","Lenah","Betsy","Nora","Shayna Two Piece","Josi Two Piece","Matteah","Harriet","Delaney Three Piece","Keaton Two Piece","Milena Two Piece","Wren Multi-way Top","Murphy","Rie","Raffy","Kito","Heston","Ada","Avanti","Oscar","Kirrily","Reverie","Vivi","Azzedine","Ekera Jumpsuit","Abberton","Olsen","Palais","Tilbury","Portrait","Cici Jumpsuit","Rosewater","Eclectic Love","Aston","Yoko","Tori Two Piece","Westside","Remy","Tate","Metz Two Piece","Prim","Harbourside","Golden Era Two Piece","Rosalita","Siana","Laced Victoria","Holy Smoke","Queen of the Night","Serpentina","Rio","Sunday Dreamer","Kimberly","Silk Dreamer","Dragon Eyes Lace","Elegant Lace","Natice Lace","Sheila","Darlo","Lace Debutante","Lucy Skirt","Heart Of Glass Skirt","Geneva Skirt","Estelle Skirt","Said It All Skirt","Spoken For Skirt","Eyes On You Skirt","Daydream Skirt","Liesl Skirt","Cenit Skirt","Merille Skirt","Milena Skirt","Metz Skirt","Seville Skirt","Elliot Skirt","Amalie Skirt","Geena Skirt","Ebony Skirt","Catania Skirt","Pretoria Skirt","Marin Skirt","Ryn Skirt","Sahara Skirt","Millie Skirt","Vicenza Skirt","Fitz Skirt","Quinn Skirt","Saba Skirt","Perrotta Skirt","Keira Skirt","Aki Skirt","Ferara Skirt","Rocha Skirt","Croix Skirt","Zuma Skirt","Elana Skirt","Sinead Skirt","Fall From Heaven Skirt","Luella Skirt","Dahlia Skirt","Krizia Skirt","Alberta","Memphis","Dream About Me","Let It Be","Beneath the Garden","Silence","York","Zeenat Skirt","Pluto","Avon","Veracruz","Raleigh","Skylight","Cheyenne","Paradox","Cairo Two Piece","Noire","Havana","Khiva","Luxor","Rize","Valletta","Zenika","Giza","Essex","Visby","Kiki Corset","Intuition Dress","Sinatra Dress","Minutiae Dress","Horus Dress","Luna Skirt","Ollie Skirt","Octavia Dress","Ace Jacket","Edge of Town Jumpsuit","Bessie Dress","Viva Skirt","Wafia Skirt","Cosmic Gate Jumpsuit","Sweet Thunder Dress","Jojo Corset","Jayah Jumpsuit","Rikki Jumpsuit","Livinia Dress","Latrice Dress","Fifi Shirt","Beckett Pant","Apollo Dress","Regis Dress","Shibuya Top","Zella Top","Kiko Kimono Coat","Midnight Chaos Dress","Athena Dress","Selene Top","Atlas Dress","Tillie Dress","Eos Dress","Hypnos Top","Margiela Pant","Hillary Pant","Alyn Dress","Allie Dress","Celia Dress","Myra Dress","Jaden Skirt","Syrah Two Piece Dress","Flora Dress","Sisilia Dress","Aurelia Dress","Axel Dress","Elite Dress","Lalitya Dress","Malika Dress","Backless Haven Dress","Backless Starlet Dress","Pritisha Dress","Nikita Dress","Alija Dress","Nella Dress","Maisie Dress","Midnight Dust Dress","Surreal Dreamer Dress","Ekko Dress","Rosie Dress","The Zelda","The Josephine","The Clara","The Fitzgerald","The Nicks","The Joni","The Kensie","The Alexandrina","The Charlotte","The Jagger","The Katharine","The Annie","The Victoria","The Sire_ne","The Fleur","The Aurore","The Deneuve","The Esme_","The Coralie","The Sylvie","The Bessette"),
    style_number = c("C161010","C161011","C161012","C161013","C161014","C161014P","C161015","C161016","C161016B","C161017","C161018","C161019","C161020","C161031","C161031B","C161032","C161033","4B218B","4B221B","4B222B","C161009","C161025","C161026","C161026B","C161027","C161029","C161030","4B498C","4B569D","4B569DP","4B571P","C161021","C161021P","C161023","C161024","C161035","C161036","C161036B","C161044","C161044B","C161046","4B487C","C161001","C161002","C161003","C161003B","C161004","C161006","C161008","C161037","C161037P","C161038","C161041","C161042P2","C161043","C161043B","C161045","C161047P","C161050","C161051","C161054","C161049","FP2003","FP2004","FP2011","FP2011P","FP2013","FP2014","FP2015P","FP2021","FP2023P","FP2030","FP2033","FP2034P","FP2035","FP2036","FP2037","C161028","FP2006","FP2006P","FP2018","FP2027P","FP2029P","FP2032","FP2039","FP2041","FP2042","FP2044","FP2045","FP2046","FP2047","FP2048","FP2049","FP2050","FP2051","FP2052","FP2053","FP2055","FP2056","FP2057","FP2059","FP2060","FP2061","FP2062","FP2063","FP2064","FP2112","FP2113","4B280SKT","4B316SKT","4B481SKT","4B500SKT","4B592LSKT","4B627BSKT","4B627SKT","4B629SKT","4B654SKT","C161008SKT","C161037PSKT","C161037SKT","C161054SKT","FP2046SKT","FP2066","FP2067","FP2068","FP2069","FP2070","FP2071B","FP2082","FP2083","FP2084","FP2085","FP2086","FP2090","FP2095","FP2099","FP2099B","FP2100","FP2102","FP2104","FP2105","FP2135","FP2160","FP2161","FP2163","USP1026SKT","USP1072SKT","USP1074PSKT","USP1099SKT","USP1231SKT","FP2020","FP2029B","FP2136","FP2136P","FP2138P","FP2153","FP2154","FP2182","FP2191","FP2191B","FP2192","FP2192B","FP2192P","FP2193","FP2195","FP2196","FP2197","FP2198","FP2199","FP2199B","FP2200","FP2201","FP2202","FP2203","FP2204","FP2208","FP2108","FP2109","FP2115","FP2118","FP2122","FP2126","FP2126B","FP2127","FP2128","FP2139","FP2142","FP2147","FP2147P","FP2158","FP2159","FP2162","FP2164","FP2164B","FP2166","FP2166B","FP2167","FP2168","FP2169","FP2170","FP2171","FP2171B","FP2172","FP2173","FP2174","FP2175","FP2176","FP2176B","FP2178","FP2179","FP2181B","FP2190","FP2198M","FP2205","FP2206","FP2207","FP2232","FP2233","FP2264","FP2265","FP2266","FP2267","FP2237","FP2238","FP2239","FP2240","FP2241","FP2242","FP2243","FP2244","FP2245","FP2246","FP2247","FP2248","FP2249","FPRV1027","FP2221","FP2222","FP2223","FP2224","FP2226","FP2227","FP2228","FP2229","FP2230","FP2231","FP2301","FP2302","FP2305","FPMC107","FPMC108","FPMC116","FPMC117","FPMC118","FPMC120","FPMC124","FPMC126")
)

products_sold <-
    tbl(fp_con, sql(paste(
        "SELECT",
            "li.id,",
            "li.order_id,",
            "o.number order_number,",
            "o.state order_state,",
            "o.shipment_state,",
            "li.quantity,",
            "li.price,",
            "o.total / (COUNT(*) OVER (PARTITION BY li.order_id)) order_total,",
            "li.currency,",
            "INITCAP(sa.city) ship_city,",
            "INITCAP(ss.name) ship_state,",
            "INITCAP(sc.name) ship_country,",
            "o.completed_at::date order_date,",
            "CASE WHEN s.ship_date IS NULL THEN o.projected_delivery_date::DATE ELSE s.ship_date::DATE END ship_date,",
            "o.email,",
            "o.user_id,",
            "INITCAP(o.user_first_name) || ' ' || INITCAP(o.user_last_name) customer_name,",
            "p.id product_id,",
            "INITCAP(p.name) style_name,",
            "ir.refund_amount IS NOT NULL item_returned,",
            "ir.refund_amount / 100 refund_amount,",
            "INITCAP(TRIM(ir.reason_category)) return_reason_extra,",
            "CASE WHEN ir.id IS NOT NULL THEN li.order_id END return_order_id,",
            "INITCAP(cust.color) color,",
            "cust.size,",
            "UPPER(v.sku) style_number,",
            "CASE WHEN cust.height IS NULL THEN 'Not Specified' ELSE INITCAP(cust.height) END height,",
            "RANK() OVER (PARTITION BY o.email ORDER BY o.completed_at) order_num,",
            "CASE WHEN tax.short_long IS NULL OR tax.short_long = 'Long,Short'",
                "THEN 'Not Specified' ELSE tax.short_long END short_long,",
            "COALESCE(tax.skirt = 1, FALSE) skirt,",
            "COALESCE(tax.evening = 1, FALSE) evening,",
            "COALESCE(tax.cocktail = 1, FALSE) cocktail,",
            "COALESCE(tax.bridal = 1, FALSE) bridal,",
            "COALESCE(tax.daytime = 1, FALSE) daytime,",
            "COALESCE(tax.jumpsuit = 1, FALSE) jumpsuit",
        "FROM spree_line_items li",
        "LEFT JOIN spree_orders o",
            "ON o.id = li.order_id",
        "LEFT JOIN spree_variants v",
            "ON v.id = li.variant_id",
        "LEFT JOIN spree_products p",
            "ON p.id = v.product_id",
        "LEFT JOIN spree_addresses sa",
            "ON sa.id = o.ship_address_id",
        "LEFT JOIN spree_states ss",
            "ON ss.id = sa.state_id",
        "LEFT JOIN spree_countries sc",
            "ON sc.id = sa.country_id",
        "LEFT JOIN item_returns ir",
            "ON ir.line_item_id = li.id",
        "LEFT JOIN (",
            "SELECT", 
                "lip.line_item_id,",
                "MAX(CASE WHEN lip.customization_value_ids SIMILAR TO '%(1|2|3|4|5|6|7|8|9|0)%'",
                    "THEN 1 ELSE 0 END) physical_customization,",
                "MAX(CASE WHEN pcv.custom THEN 1 ELSE 0 END) color_customization,",
                "STRING_AGG(DISTINCT lip.color, ',') color,",
                "STRING_AGG(DISTINCT lip.size, ',') size,",
                "STRING_AGG(DISTINCT lip.height, ',') height",
            "FROM line_item_personalizations lip",
            "LEFT JOIN product_color_values pcv",
                "ON pcv.id = lip.color_id",
            "GROUP BY line_item_id) cust",
            "ON cust.line_item_id = li.id",
        "LEFT JOIN (",
            "SELECT order_id, MAX(shipped_at::DATE) ship_date",
            "FROM spree_shipments",
            "GROUP BY order_id) s",
            "ON s.order_id = li.order_id",
        "LEFT JOIN (",
            "SELECT  ",
                "p.id product_id,",
                "STRING_AGG(DISTINCT CASE ",
                           "WHEN LOWER(t.name) SIMILAR TO '(mini|knee|petti)' THEN 'Short'",
                           "WHEN LOWER(t.name) SIMILAR TO '(midi|ankle|maxi)' THEN 'Long'",
                           "END, ',') short_long,",
                "MAX(CASE WHEN LOWER(t.name) LIKE '%skirt%' OR LOWER(p.name) LIKE '%skirt%' THEN 1 ELSE 0 END) skirt,",
                "MAX(CASE WHEN t.name = 'Evening' THEN 1 ELSE 0 END) evening,",
                "MAX(CASE WHEN t.name = 'Cocktail'THEN 1 ELSE 0 END) cocktail,",
                "MAX(CASE WHEN LOWER(t.name) LIKE '%brid%' THEN 1 ELSE 0 END) bridal,",
                "MAX(CASE WHEN t.name = 'Daytime' THEN 1 ELSE 0 END) daytime,",
                "MAX(CASE WHEN LOWER(t.name) LIKE '%jump%' THEN 1 ELSE 0 END) jumpsuit",
            "FROM spree_products_taxons pt",
            "JOIN spree_taxons t",
               "ON t.id = pt.taxon_id",
            "JOIN spree_products p",
                "ON p.id = pt.product_id",
            "GROUP BY p.id) tax",
            "ON tax.product_id = p.id",
        "WHERE o.completed_at IS NOT NULL",
            "AND o.completed_at >= '2016-01-01'",
            "AND o.state = 'complete'",
            "AND o.payment_state = 'paid'",
            "AND o.shipment_state = 'shipped'"))) %>%
    collect(n = Inf) %>%
    mutate(return_reason = ifelse(
        is.na(return_reason_extra) 
        | return_reason_extra %in% c("N/A","Na","Not Specified","Not Stated","Not Satisfied"),
        "No Reason",
        return_reason_extra)) %>%
    left_join(collections, by = "style_number") %>%
    mutate(collection = ifelse(is.null(collection_null), "Old Collection", collection_null)) %>%
    select(-collection_null) %>%
    mutate(ship_year_month = paste(year(ship_date), 
                                   formatC(month(ship_date), width = 2, flag = "0"), 
                                   sep = "-"),
           revenue_usd = order_total * ifelse(currency == "AUD", 0.75, 1),
           refund_amount_usd = refund_amount * ifelse(currency == "AUD", 0.75, 1))

same_dress <- # build list of repeat dress purchases to filter out
    products_sold %>%
    group_by(email, style_name) %>%
    mutate(order_num = rank(order_date, ties.method = "first"),
           products = n(),
           orders = n_distinct(order_id)) %>%
    arrange(order_date) %>%
    # remove all except the last purchase of repeat product purchases
    filter(order_num < max(order_num)) %>%
    ungroup()

repeat_customers <-
    unique((
        products_sold %>%
            anti_join(same_dress, by = "id") %>%
            group_by(email) %>%
            filter(!str_detect(email, "fameandpartners.com|fameprteam")) %>%
            mutate(order_num2 = dense_rank(order_num)) %>%
            filter(order_num2 > 1)
    )$email)

repeat_customers_since_jun <-
    unique((
        products_sold %>%
            anti_join(same_dress, by = "id") %>%
            filter(order_date >= as.Date("2016-06-01")) %>%
            filter(!str_detect(email, "fameandpartners.com|fameprteam")) %>%
            group_by(email) %>%
            mutate(order_num2 = dense_rank(order_num)) %>%
            filter(order_num2 > 1)
    )$email)

sales <- 
    products_sold %>%
    group_by(email) %>%
    mutate(
        order_num2 = dense_rank(order_num),
        which_order = paste0(
            ifelse(order_num2 == 1, "1st", 
                   ifelse(order_num2 == 2, "2nd",
                          ifelse(order_num2 >= 3, "3rd+",
                                 "1st")))," Purchase"),
        customer_engagement_level = 
            ifelse(email %in% repeat_customers_since_jun, 
                "Repeat Customer Since June",
                ifelse(
                    email %in% repeat_customers, 
                    "Repeat Customer", 
                    "New Customer"))) %>%
    select(-order_num) %>%
    rename(order_num = order_num2) %>%
    ungroup()
