library(mgcv)
library(tidyverse)

load("../data/Irish.RData")

cust <- Irish$indCons
extra <- Irish$extra
surv <- Irish$survey

extra$dateTime <- extra$dateTime + 60*60 #add 1 hour so tod 0 is 00:00 not 23:00

df <- data.frame(cust, extra) %>% 
  pivot_longer(names_to = "ID", c(-time, -toy, -dow, -holy, -tod, -temp, -dateTime), values_to = "demand" ) %>%
  left_join(surv, by = "ID")
