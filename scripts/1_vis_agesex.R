library(tidyverse)




country = "Brazil"


inc <- read_csv(here::here("data", country, "inc.csv"))

pop <- read_csv(here::here("data", country, "pop.csv"))


powerlaw_s <- pop %>% 
  left_join(inc %>% 
              filter(age_group == 'all' & risk_factor == 'all' & sex != 'a') %>% 
              mutate(risk = M / N) %>% 
              select(Sex = sex, risk)) %>% 
  group_by(Sex, risk) %>% 
  summarise(N = sum(N)) %>% 
  ungroup() %>% 
  mutate(Case = N * risk) %>% 
  arrange(-Case) %>% 
  select(Sex, N, Case) %>% 

  mutate(
    N1 = cumsum(N) / sum(N),
    N0 = c(0, N1[-n()]),
    Case1 = cumsum(Case) / sum(Case),
    Case0 = c(0, Case1[-n()]),
    Rank = 1:n()
  )


powerlaw_s %>% 
  ggplot() +
  geom_rect(aes(xmin=N0, xmax=N1, ymin=Case0, ymax=Case1, fill=Sex), alpha = 0.2) +
  geom_line(aes(x = N0, y = Case0)) +
  geom_line(aes(x = N1, y = Case1)) +
  scale_x_continuous("Cumulated Population, %", labels = scales::percent) +
  scale_y_continuous("cumulated Risk, %", labels = scales::percent)




powerlaw_as <- pop %>% 
  mutate(
    AgeGroup = case_when(
      Age < 15 ~ "0-14",
      Age < 25 ~ "15-24",
      Age < 35 ~ "25-34",
      Age < 45 ~ "35-44",
      Age < 55 ~ "45-54",
      Age < 65 ~ "55-64",
      T ~ "65plus"
    )
  ) %>% 
  left_join(
    inc %>% 
      filter(risk_factor == 'all' & age_group %in% c("0-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65plus")) %>%
      filter(sex != "a") %>% 
      mutate(risk = M / N) %>% 
      select(Sex = sex, AgeGroup = age_group, risk)
  ) %>% 
  group_by(AgeGroup, Sex, risk) %>% 
  summarise(N = sum(N)) %>% 
  ungroup() %>% 
  mutate(Case = N * risk) %>% 
  arrange(-Case) %>% 
  select(AgeGroup, Sex, N, Case) %>% 
  mutate(
    N1 = cumsum(N) / sum(N),
    N0 = c(0, N1[-n()]),
    Case1 = cumsum(Case) / sum(Case),
    Case0 = c(0, Case1[-n()]),
    Rank = 1:n()
  )



powerlaw_as %>% 
  ggplot() +
  geom_rect(aes(xmin=N0, xmax=N1, ymin=Case0, ymax=Case1, fill=Sex, alpha = AgeGroup)) +
  geom_line(aes(x = N0, y = Case0)) +
  geom_line(aes(x = N1, y = Case1)) +
  scale_x_continuous("Cumulated Population, %", labels = scales::percent) +
  scale_y_continuous("cumulated Risk, %", labels = scales::percent)



powerlaw_s %>% 
  ggplot() +
  geom_rect(aes(xmin=N0, xmax=N1, ymin=Case0, ymax=Case1, fill=Sex), alpha = 0.2) +
  geom_line(aes(x = N0, y = Case0)) +
  geom_line(aes(x = N1, y = Case1)) +
  geom_rect(data = powerlaw_as, aes(xmin=N0, xmax=N1, ymin=Case0, ymax=Case1, fill=Sex, alpha = AgeGroup)) +
  geom_line(data = powerlaw_as, aes(x = N0, y = Case0)) +
  geom_line(data = powerlaw_as, aes(x = N1, y = Case1)) +
  geom_text(data = powerlaw_as, aes(x = N1, y = (Case0 + Case1) / 2, label = AgeGroup, hjust=0)) +
  geom_abline(slope = 1, linetype = 2) +
  scale_x_continuous("Cumulated Population, %", labels = scales::percent) +
  scale_y_continuous("cumulated Risk, %", labels = scales::percent)





opt <- nlm(function(x) {
  powerlaw_s %>% 
    mutate(
      h = 1 / Rank ^ x,
      h = cumsum(h) / sum(h),
      v = (h - Case1) ^ 2
    ) %>% 
    pull(v) %>% 
    sum()
}, p = 2)



opt

fn_pl <- function(s) {
  x <- 1:100
  y <- 1 / (x) ^ s
  y <- cumsum(y) / sum(y)
  approxfun(x, y, yleft = 0, yright = 1)
}


pl = fn_pl( 1.258083e-12)

powerlaw_s %>% 
  mutate(
    h = pl(N1 * 100)
  )


opt <- nlm(function(x) {
  pl = fn_pl(x)
  
  ss = powerlaw_s %>% 
    mutate(
      h = pl(N1 * 100),
      v = (h - Case1) ^ 2
    )
  
  print(ss$v)
  
  ss %>% 
    pull(v) %>% 
    sum()
}, p = 2)


optimise(function(p, ns, cases) {
  x <- 1:100
  y <- 1 / (x) ^ p
  y <- cumsum(y) / sum(y)
  pl <- approx(x / 100, y, xout=ns, yleft = 0, yright = 1)$y
  sum((pl / cases - 1)^2)
},
interval = c(0, 10), ns = powerlaw_s$N1, cases = powerlaw_s$Case1
)


