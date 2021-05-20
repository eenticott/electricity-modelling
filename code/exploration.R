# Initial play around with the data, making plots and looking at demand correlations

source("code/data_prep2.R")

# Define function to plot any demand data as a 12 panel plot
plot_monthly_usage <- function(cust, cust_ID, extra){
  
  df <- extra %>% 
    mutate(demand = cust[, cust_ID],
           month = factor(month(dateTime), levels=1:12, labels=format(ISOdate(2004,1:12,1),"%b")),
           time_format = as.POSIXct(strftime(dateTime, format="%H:%M:%S"), format="%H:%M:%S"),
           weekend = ifelse(dow %in% c("Sat", "Sun"), "Weekend", "Weekday"))
  
  p <- ggplot(df, aes(x=time_format, y=demand, color=month, group=date)) +
    geom_line(aes(lty=weekend), lwd=0.15) +
    facet_wrap(~month) +
    theme_minimal() +
    theme(legend.position="bottom",
          axis.text.x = element_text(angle = 90)) +
    labs(title=paste(cust_ID, "Customer Profile"), x="\nTime of day", y="Demand (kWh)") +
    scale_x_datetime(date_breaks = "2 hour", date_labels = "%H:%M", 
                     limits=range(df$time_format), expand=c(0,0)) +
    guides(color = FALSE, 
           lty = guide_legend(override.aes = list(lwd=0.5), title=NULL))
  
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
#ggsave("report/TotalUsage.png", width=8, height=6)


# Look how different parameters correlate with mean household consumption
par(mfrow=c(3, 3))
par(mar=c(2,2,2,1)) 
for(i in 3:ncol(surv)){
  var <- as.numeric(surv[,i])
  cor <- round(cor(surv$meanDem, var), 2)
  title <- paste(colnames(surv)[i], "r =", cor)
  plot(var, surv$meanDem, main=title, pch=20, cex=0.8)
} 
#low correlations other than white goods
#possible some factors should be excluded or 1-hot encoded

# Look how the timing parameters correlate with demand
df <- data.frame(cust[1:100], extra) %>% 
  pivot_longer(names_to = "ID", c(-time, -toy, -dow, -tod, -temp, -dateTime, -date), values_to = "demand") %>%
  select(-time, -dateTime) %>%
  sample_n(5000) %>%
  as.data.frame()

par(mfrow=c(2, 2))
for(i in 1:4){
  var <- as.numeric(df[,i])
  cor <- round(cor(df$demand, var), 2)
  title <- paste(colnames(df)[i], "r =", cor)
  plot(var, df$demand, main=title, pch=20, cex=0.8)
}
#clear non-linear relationship with time of year and time of day
#surprisingly low correlation with temp - poss because most of the houses have non-electric heating
#and generally low demand at night when its colder

ggplot(df, aes(x=toy, y=demand)) + 
  geom_smooth(method="loess", se=F) +
  geom_jitter(size=0.1, alpha=0.5)

ggplot(df, aes(x=tod, y=demand)) + 
  geom_smooth(method="loess", se=F) +
  geom_jitter(size=0.1, alpha=0.5)

#definitely this exploration shows that we need to use a feature transform - at the very least squared toy and tod

#if we have 13 parameters, including all 2nd and 3rd order terms + 2nd order interactions gives 13*3+13C2=117 
#probably should one-hot encode non-sequential factors, only do higher orders for numerical variables, and avoid adding highly correlated variables
#for a single household we have 16799 total observations, or ~350 per time of day


# Check what days have been removed
dates <- unique(extra$date)
all_dates <- seq(min(dates), max(dates), by="days")
missing <- all_dates[!all_dates %in% dates]
format(missing, "%d/%m/%y")

#dates like Christmas, boxing day, Easter Sunday and Easter Monday (4th-5th April) have been removed
#unclear on all of them e.g. why is the 23-24th May are removed and and 2nd April (Good Friday) not 
#note in data prep we removed 2 additional dates