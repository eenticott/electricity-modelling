# Some initial data exploration
library(tidyverse)
library(lubridate)
load("../data/Irish.RData")

cust <- Irish$indCons
extra <- Irish$extra
surv <- Irish$survey

extra$dateTime <- extra$dateTime + hours(1)

#function to plot any customers data as a 12 panel plot
plot_monthly_usage <- function(cust, cust_ID, extra){
  
  df <- extra %>% 
    mutate(demand = cust[, cust_ID],
           date = as.factor(as.Date(dateTime)), 
           month = factor(month(dateTime), levels=1:12, labels=format(ISOdate(2004,1:12,1),"%b")),
           time_format = as.POSIXct(strftime(dateTime, format="%H:%M:%S"), format="%H:%M:%S"))
  
  p <- ggplot(df, aes(x=time_format, y=demand, color=month, group=date)) +
    geom_line(lwd=0.1) +
    facet_wrap(~month) +
    theme_minimal() +
    theme(legend.position="none",
          axis.text.x = element_text(angle = 90)) +
    labs(title=paste(cust_ID, "Customer Profile"), x="Time of day", y="Demand (kWh)") +
    scale_x_datetime(date_breaks = "2 hour", date_labels = "%H:%M", 
                     limits=range(df$time_format), expand=c(0,0))
  
  return(p)
}

#applying to random customer
set.seed(27)
cust_ID <- colnames(cust)[sample(1:ncol(cust), 1)]
plot_monthly_usage(cust, cust_ID, extra)

#applying to total usage
totals <- data.frame("Total" = rowSums(cust))
plot_monthly_usage(cust=totals, cust_ID="Total", extra) +
  labs(title=NULL)
ggsave("../report/TotalUsage.png", width=8, height=6)
#maybe colour by day of week? doesnt look as nice though




