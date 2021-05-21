# Load the data and do some simple preprocessing

library(tidyverse)
library(lubridate)

load("data/Irish.RData")

cust <- Irish$indCons
extra <- Irish$extra
surv <- Irish$survey

#add 1 hour so tod 0 is 00:00 not 23:00
extra$dateTime <- extra$dateTime + 60*60 

#remove surv$Code and extra$holy since they are constant
surv$Code <- NULL
extra$holy <- NULL

#add a date column
extra$date <- as.Date(extra$dateTime, tz="Europe/Dublin")

#remove last day of 2009 and new years eve 2010
idx <- extra$date %in% as.Date(c("2010-12-31", "2009-12-30"))
cust <- cust[!idx,]
extra <- extra[!idx,]
rm(idx)

#reorder day of the week factor levels
extra$dow <- factor(extra$dow, levels=c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

#make survey answers factors (and nicer names)
surv <- rename(surv, 
               class = SOCIALCLASS,
               own = OWNERSHIP,
               built = BUILT.YEAR,
               heatHome = HEAT.HOME,
               heatWater = HEAT.WATER,
               windows = WINDOWS.doubleglazed,
               whiteGoods = HOME.APPLIANCE..White.goods.,
               tariff = ResTariffallocation,
               stimulus = ResStimulusallocation) 

surv$own <- as.factor(surv$own) #2 levels
surv$heatHome <- as.factor(surv$heatHome) #2 levels
surv$heatWater <- as.factor(surv$heatWater) #2 levels
surv$windows <- factor(surv$ windows, levels=c("None", "Quarter", "HalfQuarters", "Half", "All"))

#add a column to extra indicating the last day of the month to use as a hold-out validation set
extra <- extra %>%
  mutate(month = month(date)) %>%
  group_by(month) %>%
  mutate(lastDay = max(date)) %>%
  ungroup() %>%
  mutate(testSet = date==lastDay) %>%
  select(-lastDay, -month)


# maybe this only needs to be created per model, since its so large?

# df <- data.frame(cust, extra) %>% 
#   pivot_longer(names_to = "ID", c(-time, -toy, -dow, -holy, -tod, -temp, -dateTime), values_to = "demand" ) %>%
#   left_join(surv, by = "ID")

