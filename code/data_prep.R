library(mgcv)
library(tidyverse)

load("data/Irish.RData")

cust <- Irish$indCons
extra <- Irish$extra
surv <- Irish$survey

df <- data.frame(cust, extra) %>% 
  pivot_longer(names_to = "ID", c(-time, -toy, -dow, -holy, -tod, -temp, -dateTime), values_to = "demand" ) %>%
  left_join(surv, by = "ID")
