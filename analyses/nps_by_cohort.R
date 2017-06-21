cohort_assignments %>% 
    inner_join(nps %>% 
                   rename(email = Email), 
               by = "email") %>% 
    mutate(nps_level = ifelse(Score >= 9, "Promoters", 
                              ifelse(Score >= 7, "Passives", 
                                     "Detractors"))) %>% 
    count(cohort, nps_level) %>% 
    mutate(percent_of_total = n / sum(n)) %>% 
    select(-n) %>% 
    spread(nps_level, percent_of_total) %>% 
    mutate(`NPS Score` = (Promoters - Detractors) * 100) %>% 
    rename(Cohort = cohort) %>% 
    write_csv("~/data/nps_by_cohort.csv")