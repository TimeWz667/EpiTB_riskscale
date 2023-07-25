library(tidyverse)





country = "India"
sc = "cluster"


ir0 <- read_csv(here::here("data", "inc_" + glue::as_glue(country) + "_" + glue::as_glue(sc) + ".csv"))

ir1 <- bind_rows(
  ir0 %>% filter(risk_factor != "all"),
  ir0 %>% 
    filter(risk_factor == "all") %>% 
    filter(sex != "a") %>% 
    filter(age_group %in% c("0-14", "15-24", "25-34", "35-44", "45-54", "5-14", "55-64", "65plus"))
)




fit <- glm(M~ Smk * Alc + Und + Dia + HIV + age_group + sex + offset(log(N)), data = ir1, family = poisson())

summary(fit)
