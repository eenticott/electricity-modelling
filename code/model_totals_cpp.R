# Model totals but with Rcpp functions and make a plot

source("code/data_prep2.R")
load("data/TotalsTest.RData")
library(Rcpp)
library(RcppArmadillo)
Rcpp::sourceCpp("code/ridge_reg.cpp")

#initialise
M <- 30
lambdas <- exp(seq(-1,5,length=M))
ocvs <- rep(NA, M)
y_pred <- rep(NA, length(y_test))


#fit a model per tod and use it to predict the last day
for(i in 1:48){
  #subset the training data
  tod <- X_test[i, "tod"]
  X_sub <- X[X[,"tod"] == tod,]
  y_sub <- as.matrix(y[X[,"tod"] == tod])
  
  #find lambda hat through OCV
  ocvs <- optim_rr(X_sub, y_sub, lambdas)
  l_hat <- lambdas[which.min(ocvs)]
  #plot(log(lambdas), ocvs)
  
  #get optimal betas
  beta <- fit_rr(X_sub, y_sub, l_hat)
  
  #use optimal betas to predict the hold out day
  test_sub <- X_test[,"tod"] == tod
  y_pred[test_sub] <- X_test[test_sub,] %*% beta
}

y_test <- y_test + mean_y
y_pred <- y_pred + mean_y

plot(y_test, type="l", ylim=range(y_test, y_pred))
lines(y_pred, col=2)

(MSE <- mean(abs(y_test-y_pred)))


#adapting the plot from before to look at these results
df <- extra %>% 
  filter(df$testSet==T) %>%
  mutate(TrueDemand=y_test,
         PredictedDemand=y_pred, 
         month = factor(month(dateTime), levels=1:12, labels=format(ISOdate(2004,1:12,1),"%b")),
         time_format = as.POSIXct(strftime(dateTime, format="%H:%M:%S"), format="%H:%M:%S")) %>%
  pivot_longer(cols=c("TrueDemand", "PredictedDemand"), values_to="demand", names_to="type") %>%
  mutate(group=paste(type, date), type=factor(type, levels=c("TrueDemand", "PredictedDemand")))

ggplot(df, aes(x=time_format, y=demand, color=month, group=group)) +
  geom_line(aes(lty=type)) +
  facet_wrap(~month) +
  theme_minimal() +
  theme(legend.position="bottom",
        axis.text.x = element_text(angle = 90)) +
  labs(title="Model Predictions", x="\nTime of day", y="Demand (kWh)") +
  scale_x_datetime(date_breaks = "2 hour", date_labels = "%H:%M", 
                   limits=range(df$time_format), expand=c(0,0)) +
  guides(color = FALSE, lty = guide_legend(title=NULL))
ggsave("report/TotalUsagePredictions.png", width=8, height=6)






