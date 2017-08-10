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
    summarise(Units = sum(quantity)) %>%
    mutate(Units / sum(Units))
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
# p-value = 0.7357
# Plus sized styles are just as likely to be returned for a size and fit reason
# as any other order

live_style_sales %>%
    mutate(`Return Reason` = toupper(return_reason)) %>%
    filter(return_requested & `Return Reason` != "NO REASON") %>%
    group_by(sizing = ifelse(plus_sized, "14+", "0-12"), `Return Reason`) %>%
    summarise(Units = sum(quantity)) %>%
    mutate(percent_of_returns = Units / sum(Units)) %>%
    select(-Units) %>%
    spread(sizing, percent_of_returns) %>%
    write_csv("~/data/plus_size_returns.csv")
# The differences in return reasons for plus sized styles are minimal

return_reasons <- products_sold %>%
    filter(return_requested) %>%
    group_by(order_year = year(order_date), reason = toupper(return_reason)) %>%
    summarise(returns = sum(quantity)) %>%
    filter(order_year > 2015 & returns > 10) %>%
    mutate(perc_of_returns = returns / sum(returns))

return_reasons %>%
    arrange(order_year, desc(reason)) %>%
    mutate(position = cumsum(returns)) %>%
    ggplot(aes(x = order_year, y = returns, fill = reason)) +
    geom_bar(stat = "identity", position = "fill") +
    geom_label(aes(label = percent(perc_of_returns), y = perc_of_returns),
               position = "stack") +
    scale_fill_brewer(palette = "Set3") +
    scale_x_continuous(breaks = c(2016,2017)) +
    scale_y_continuous(labels = percent) +
    ylab("Percent of Returns") +
    theme_minimal(base_size = 14) +
    theme(axis.title.x = element_blank(),
          legend.title = element_blank())

return_reasons %>%
    select(-returns) %>%
    spread(order_year, perc_of_returns, fill = 0) %>%
    mutate(`2016 to 2017` = `2017` / `2016` - 1)

# Is there a size that has a significantly high size and fit return rate?
live_style_sales %>%
    group_by(us_size) %>%
    summarise(sf_return_rate = sum(size_fit * quantity) / sum(quantity),
              sf_returns = sum(size_fit * quantity),
              units = sum(quantity)) %>%
    arrange(desc(sf_return_rate)) %>%
    filter(sf_return_rate > 0.25)
# us_size sf_return_rate sf_returns units
# 16      0.2718447      112        412
binom.test(112, 412, p = 0.2396858, "t")
# p-value = 0.1334

# Are there any collections that have a significantly high size and fit return 
# rate?
live_style_sales %>%
    group_by(collection) %>%
    summarise(sf_return_rate = sum(size_fit * quantity) / sum(quantity),
              sf_returns = sum(size_fit * quantity),
              units = sum(quantity)) %>%
    arrange(desc(sf_return_rate)) %>%
    filter(sf_return_rate > 0.24)
# collection    sf_return_rate sf_returns units
# 2016 - Prom      0.2896904       1020  3521
binom.test(1020, 3521, p = 7140 / 29789, "t")
# Yes the 2016 Prom collection has a size and fit return rate of 29%


# Are there any styles that have a significantly high size and fit return rate?
sf_problem_styles <- live_style_sales %>%
    filter(year(order_date) == 2017) %>%
    group_by(style_number) %>%
    summarise(sf_return_rate = sum(size_fit * quantity) / sum(quantity),
              sf_returns = sum(size_fit * quantity),
              units = sum(quantity)) %>%
    filter(sf_return_rate > 0.35) %>%
    arrange(desc(sf_return_rate))

sf_problem_styles$binom_pval <- sf_problem_styles %>%
    select(sf_returns, units) %>%
    apply(1, function(x){
        binom.test(x[1], x[2], p = 7140 / 29789, "t")$p.value
    })

bad_styles <- sf_problem_styles %>%
    filter(binom_pval < 0.05) %>%
    inner_join(products %>% 
                   select(style_number, product_id, style_name, available_on), 
               by = "style_number") %>%
    inner_join(collections, by = "product_id")

# Was there a particular launch date that has had substantially high size and
# fit return request rate?
sf_problem_launches <- live_style_sales %>%
    filter(year(order_date) == 2017) %>%
    group_by(launch_date = as.Date(available_on)) %>%
    summarise(sf_return_rate = sum(size_fit * quantity) / sum(quantity),
              sf_returns = sum(size_fit * quantity),
              units = sum(quantity)) %>%
    filter(sf_return_rate > 0.35) %>%
    arrange(desc(sf_return_rate))

sf_problem_launches$binom_pval <- sf_problem_launches %>%
    select(sf_returns, units) %>%
    apply(1, function(x){
        binom.test(x[1], x[2], p = 7140 / 29789, "t")$p.value
    })

bad_launches <- sf_problem_launches %>%
    filter(binom_pval < 0.05)
# Yes the launch on 2016-04-03 has a size and fit return rate of 31%

# Are there any style, height and length combinations with a high size and fit
# return request rate?
style_length_heights <- live_style_sales %>% 
    group_by(style_number, length, height) %>%
    summarise(sf_return_rate = sum(size_fit * quantity) / sum(quantity),
              sf_returns = sum(size_fit * quantity),
              units = sum(quantity)) %>%
    filter(sf_return_rate > 0.35) %>%
    arrange(desc(sf_return_rate)) %>%
    ungroup()

style_length_heights$binom_pval <- style_length_heights %>%
    select(sf_returns, units) %>%
    apply(1, function(x){
        binom.test(x[1], x[2], p = 7140 / 29789, "t")$p.value
    })

bad_style_length_heights <- style_length_heights %>%
    filter(binom_pval < 0.025 & !is.na(length) & !is.na(height) & units > 5)

height_lengths <- read_csv("~/data/sales data chart - monica.csv")

long_return_rate <- sum(height_lengths$`Units too Long`) / sum(height_lengths$Units) # 0.03247294
short_return_rate <- sum(height_lengths$`Units too Short`) / sum(height_lengths$Units) # 0.01686095

height_lengths$long_pval <- height_lengths %>%
    select(`Units too Long`, Units) %>%
    apply(1, function(x){
        binom.test(x[1], x[2], 
                   p = long_return_rate, 
                   "t")$p.value
    })

height_lengths$short_pval <- height_lengths %>%
    select(`Units too Short`, Units) %>%
    apply(1, function(x){
        binom.test(x[1], x[2], 
                   p = short_return_rate, 
                   "t")$p.value
    })

height_lengths %>%
    filter((long_pval < 0.05 & `% too Long` > 0.04) | 
               (short_pval < 0.05 & `% too Short` > 0.03))
# No height length combo has a high % too long or too short

height_lengths %>%
    group_by(`New Heights` = str_detect(Height, "Length")) %>%
    summarise(`Total Units` = sum(Units),
              `Returned Units` = round(sum(Units * `Return Request Rate`))) %>%
    mutate(`Return Rate` = `Returned Units` / `Total Units`)
# There is no difference in return rates for the new heights

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

set.seed(69)
customer_survey_sizes <- products_sold %>%
    filter(!is.na(us_size)) %>%
    sample_n(nrow(fit_size)) %>%
    transmute(size = us_size, sample = "Customers") %>%
    rbind(fit_size %>% select(size) %>% mutate(sample = "Survey"))

customer_survey_sizes %>%
    ggplot(aes(size)) +
    geom_histogram(binwidth = 2) +
    facet_grid(.~sample) +
    scale_x_continuous(breaks = seq(2, 26, 2)) +
    theme_minimal(base_size = 16) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank())

customer_survey_sizes %>%
    group_by(sample) %>%
    summarise(mean(size), median(size))

write_csv(bad_styles, "~/data/bad_styles.csv")
