setwd("~/code/analytics/ecommerce-performance")
source("~/code/analytics/ecommerce-performance/global.R")
setwd("~/data")

sales_by_cohort_collection <- products_sold %>% 
    filter(year(order_date) == 2017 & collection %>% str_detect("2017")) %>% 
    group_by(cohort = coalesce(as.character(assigned_cohort), "Not Assigned"), collection) %>% 
    summarise(units_sold = sum(quantity)) %>% 
    arrange(cohort, desc(units_sold)) %>% 
    mutate(percent_of_total = units_sold / sum(units_sold))

sorted_not_assigned <- sales_by_cohort_collection %>%
    filter(cohort == "Not Assigned") %>%
    arrange(units_sold)

sales_by_cohort_collection$collection <- factor(
    sales_by_cohort_collection$collection,
    levels = sorted_not_assigned$collection
)

sales_by_cohort_collection %>%
    filter(cohort != "Not Assigned") %>%
    ggplot(aes(x = collection, y = percent_of_total, fill = cohort)) +
    geom_bar(stat = "identity", position = "dodge") + 
    scale_fill_brewer(palette = "Set2") +
    scale_y_continuous(labels = percent) +
    ylab("Percent of Cohort Sales") +
    coord_flip() +
    theme_minimal() +
    theme(axis.title.y = element_blank(),
          legend.title = element_blank())
