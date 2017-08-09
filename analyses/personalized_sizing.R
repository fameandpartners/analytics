# Initialize Data Warehouse
source("~/code/analytics/etl/full_global.R")

live_style_sales <- products_sold %>%
    filter(product_live == "Yes" 
           & is_shipped 
           & !is.na(size)) %>%
    mutate(plus_sized = us_size >= 14,
           size_fit = tolower(return_reason) == "size and fit",
           refunded = !is.na(refund_amount))

# Do plus sized get returned more or less frequently than other orders?
live_style_sales %>% 
    group_by(return_requested) %>%
    summarise(Units = sum(quantity))
# return_requested Units
# 1            FALSE 18111
# 2             TRUE 11678
live_style_sales %>%
    group_by(plus_sized, return_requested) %>%
    summarise(Units = sum(quantity)) %>%
    mutate(Units / sum(Units))
# plus_sized return_requested Units `Units/sum(Units)`
# 1      FALSE            FALSE 17170          0.6060071
# 2      FALSE             TRUE 11163          0.3939929
# 3       TRUE            FALSE   941          0.6462912
# 4       TRUE             TRUE   515          0.3537088

binom.test(515, 1456, p = 11678 / 29789, "t")
# p-value = 0.002642
# Plus sized orders are slightly less - 4% - likely to be returned

# Do plus sized get returned with a size and fit reason more or less frequently 
# than other orders?
live_style_sales %>% 
    group_by(size_fit) %>%
    summarise(Units = sum(quantity)) %>%
    mutate(Units / sum(Units))
# size_fit Units
# 1    FALSE 22649
# 2     TRUE  7140
live_style_sales %>%
    group_by(plus_sized, size_fit) %>%
    summarise(Units = sum(quantity)) %>%
    mutate(Units / sum(Units))
# plus_sized size_fit Units `Units/sum(Units)`
# 1      FALSE    FALSE 21536          0.7601031
# 2      FALSE     TRUE  6797          0.2398969
# 3       TRUE    FALSE  1113          0.7644231
# 4       TRUE     TRUE   343          0.2355769
binom.test(343, 1456, p = 7140 / 29789, "t")
# p-value = 0.002642
# Plus sized styles are just as likely to be returned for a size and fit reason
# as any other order

live_style_sales %>%
    filter(return_requested) %>%
    group_by(sizing = ifelse(plus_sized, "14+", "0-12"), 
             `Return Reason` = tolower(return_reason)) %>%
    summarise(Units = sum(quantity)) %>%
    mutate(percent_of_returns = Units / sum(Units)) %>%
    select(-Units) %>%
    spread(sizing, percent_of_returns) %>%
    mutate(`% Change` = `14+` / `0-12` - 1)

# Is there a size that has a significantly high size and fit return rate?
live_style_sales %>%
    group_by(us_size) %>%
    summarise(sf_return_rate = sum(size_fit * quantity) / sum(quantity),
              sf_returns = sum(size_fit * quantity),
              returns = sum(quantity * return_requested),
              return_rate = returns / sum(quantity),
              units = sum(quantity)) %>%
    arrange(desc(sf_return_rate))
binom.test(106, 401, p = 7140 / 29789, "t")
# No individual size has a significantly high size and fit return rate
binom.test(2246, 5469, p = 11678 / 29789, "t")
binom.test(3047, 7558, p = 11678 / 29789, "t")
# Size 2 has a significanly high return request rate but just barely

# Are there any collections that have a significantly high size and fit return 
# rate?
live_style_sales %>%
    group_by(collection) %>%
    summarise(sf_return_rate = sum(size_fit * quantity) / sum(quantity),
              sf_returns = sum(size_fit * quantity),
              units = sum(quantity)) %>%
    arrange(desc(sf_return_rate))
# collection    sf_return_rate sf_returns units
# 2016 - Prom      0.2896904       1020  3521
binom.test(1020, 3521, p = 7140 / 29789, "t")

# Are there any styles that have a significantly high size and fit return rate?
sf_problem_styles <- live_style_sales %>%
    filter(year(order_date) == 2017) %>%
    group_by(style_number) %>%
    summarise(sf_return_rate = sum(size_fit * quantity) / sum(quantity),
              sf_returns = sum(size_fit * quantity),
              units = sum(quantity)) %>%
    filter(sf_return_rate > 0.3) %>%
    arrange(desc(sf_return_rate))

sf_problem_styles$binom_pval <- sf_problem_styles %>%
    select(sf_returns, units) %>%
    apply(1, function(x){
        binom.test(x[1], x[2], p = 7140 / 29789, "t")$p.value
    })

sf_problem_styles %>%
    filter(binom_pval < 0.05)

# Was there a particular launch date that has had substantially high size and
# fit return request rate?
sf_problem_launches <- live_style_sales %>%
    filter(year(order_date) == 2017) %>%
    group_by(launch_date = as.Date(available_on)) %>%
    summarise(sf_return_rate = sum(size_fit * quantity) / sum(quantity),
              sf_returns = sum(size_fit * quantity),
              units = sum(quantity)) %>%
    filter(sf_return_rate > 0.3) %>%
    arrange(desc(sf_return_rate))

sf_problem_launches$binom_pval <- sf_problem_launches %>%
    select(sf_returns, units) %>%
    apply(1, function(x){
        binom.test(x[1], x[2], p = 7140 / 29789, "t")$p.value
    })

bad_launches <- sf_problem_launches %>%
    filter(binom_pval < 0.05)

products %>% 
    filter(as.Date(available_on) %in% bad_launches$launch_date) %>%
    filter(!hidden 
           & (is.na(deleted_at) | deleted_at > today()) 
           & (available_on <= today()))

# Personalized sizing survey data
size_survey <- read_csv("~/data/20170725181224-SurveyExport.csv") %>%
    mutate(fit = `What percentage of the clothes in your wardrobe fit you well`,
           love = `What percentage of the clothes in your wardrobe do you love?`,
           size = `Dress Size Type 1:What's your typical dress clothing size?` %>%
               str_extract_all("[0-9]+") %>% 
               as.numeric())

size_survey %>%
    ggplot(aes(fit, love)) +
    geom_point() +
    geom_smooth(method = "lm", level = 0)

fl_model <- lm(data = size_survey, love~fit)
summary(fl_model)
cor(size_survey$fit, size_survey$love)


fit_size <- size_survey %>% filter(!is.na(size))

fit_size %>%
    ggplot(aes(size)) +
    geom_histogram(binwidth = 2)

products_sold %>%
    ggplot(aes(us_size)) +
    geom_histogram(binwidth = 2)

height_lengths <- read_csv("~/data/sales data chart - monica.csv")

height_lengths$long_pval <- height_lengths %>%
    select(`Units too Long`, Units) %>%
    apply(1, function(x){
        binom.test(x[1], x[2], 
                   p = sum(height_lengths$`Units too Long`) / sum(height_lengths$Units), 
                   "t")$p.value
    })

height_lengths$short_pval <- height_lengths %>%
    select(`Units too Short`, Units) %>%
    apply(1, function(x){
        binom.test(x[1], x[2], 
                   p = sum(height_lengths$`Units too Short`) / sum(height_lengths$Units), 
                   "t")$p.value
    })

height_lengths %>%
    filter((long_pval < 0.05 & `% too Long` > 0.03247294) | 
               (short_pval < 0.05 & `% too Short` > 0.01686095)
    )

