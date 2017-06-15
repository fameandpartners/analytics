source("~/code/analytics/etl/full_global.R")

customer_aquisitions <- products_sold %>%
    group_by(email) %>%
    summarise(date = min(order_date))

quarterly_cohorts <- products_sold %>%
    inner_join(customer_aquisitions, by = "email") %>%
    mutate(quarters_since_aquisition = paste("q",
                                             floor(difftime(order_date, 
                                                            date, 
                                                            units = "weeks") %>%
                                                       as.integer() / 12) %>%
                                                 formatC(width=2,flag="0"), 
                                             sep = "_"),
           aquisition_quarter = paste(year(date),
                                      quarter(date))) %>%
    group_by(aquisition_quarter, quarters_since_aquisition) %>%
    summarise(customers = n_distinct(email)) %>%
    group_by(aquisition_quarter) %>%
    mutate(percent_of_cohort = paste0(round(100*(customers / max(customers)), 2),"%"))

quarterly_cohorts %>%
    select(-percent_of_cohort) %>%
    spread(quarters_since_aquisition, customers, fill = 0)

quarterly_cohorts %>%
    select(-customers) %>%
    spread(quarters_since_aquisition, percent_of_cohort, fill = 0)

write_csv(customer_aquisitions, "~/code/analytics/ecommerce-performance/static-data/customer_aquisitions.csv")
