library(tidyverse)
library(dplyr)
library(lubridate)
library(smwrBase)
library(AICcmodavg)
library(graphics)

dayflow <- readr::read_csv("C:/Users/estumpne/Documents/R/drought/dayflow_1997_2020.csv")


# Create a new data frame with the newly formatted date field and calc monthly mean - same steps taken with log transformed data later in code ~line 300

dayflow_month <- dayflow %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  group_by(Date=floor_date(Date, "month")) %>%
  summarise(Year = mean(Year), 
                    Month = mean(Month),
                    mean_TOT = mean(TOT),
                    mean_OUT = mean(OUT),
                    mean_EXPORT = mean(EXPORTS), 
                    mean_X2 = mean(X2))

#subset time period

dayflow_month <- dayflow_month %>%
  filter(Date >= as.Date('2010-12-01') & Date <= as.Date('2020-09-01'))

#add water year column (Dec - Nov)

wtr_yr <- function(dates, start_month=12) {
  # Convert dates into POSIXlt
  dates.posix = as.POSIXlt(dates)
  # Year offset
  offset = ifelse(dates.posix$mon >= start_month - 1, 1, 0)
  # Water year
  adj.year = dates.posix$year + 1900 + offset
  # Return the water year
  adj.year
}

dayflow_WY <- dayflow_month %>% 
  mutate(wtr_yr = wtr_yr(Date))

#===============================================Analysis of inflow

one.way <- aov(mean_TOT ~ wtr_yr, data = dayflow_WY)

summary(one.way)

two.way <- aov(mean_TOT ~ wtr_yr + Month, data = dayflow_WY)

summary(two.way)

#Metric ~ factor(year) + month + region

#test for interaction

two.way.inter <- aov(mean_TOT ~ wtr_yr * Month, data = dayflow_WY)

summary(two.way.inter)

#find best fit model - model with lowest AIC score is the best fit for the data

model.set <- list(one.way, two.way, two.way.inter)
model.names <- c("one.way", "two.way", "two.way.inter")

aictab(model.set, modnames = model.names)

#check for homoscedasticity diagnostic plot show unexplained variance (residuals) across the range of observed data. The red line represents mean of residuals and it should be horiz. and centered at zero (or on 1 at the scale-location plot) The normal Q-Q plot plots a regression between the theoretical residuals of a perfectly-homoscedastic model and the actual residuals of your model, so the closer to a slope of 1 this is the better. These diagnostic plots do not show the model fits the assumption of homoscedasticity. I should try the Kruskall-Wallis test

par(mfrow=c(2,2))
plot(two.way)
par(mfrow=c(1,1))

#next do a post-hoc test

?TukeyHSD

tukey.two.way<-TukeyHSD(two.way)

tukey.two.way


#plot results in a graph

tukey.plot.aov <- aov(mean_TOT ~ wtr_yr + Month, data = dayflow_WY)
tukey.plot.test<-TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test, las = 1)

#===============================================Analysis of outflow

one.way <- aov(mean_OUT ~ wtr_yr, data = dayflow_WY)

summary(one.way)

two.way <- aov(mean_OUT ~ wtr_yr + Month, data = dayflow_WY)

summary(two.way)

#Metric ~ factor(year) + month + region

#test for interaction

two.way.inter <- aov(mean_OUT ~ wtr_yr * Month, data = dayflow_WY)

summary(two.way.inter)

#find best fit model - model with lowest AIC score is the best fit for the data

model.set <- list(one.way, two.way, two.way.inter)
model.names <- c("one.way", "two.way", "two.way.inter")

aictab(model.set, modnames = model.names)

#check for homoscedasticity diagnostic plot show unexplained variance (residuals) across the range of observed data. The red line represents mean of residuals and it should be horiz. and centered at zero (or on 1 at the scale-location plot) The normal Q-Q plot plots a regression between the theoretical residuals of a perfectly-homoscedastic model and the actual residuals of your model, so the closer to a slope of 1 this is the better. These diagnostic plots do not show the model fits the assumption of homoscedasticity. I should try the Kruskall-Wallis test

par(mfrow=c(2,2))
plot(two.way)
par(mfrow=c(1,1))

#next do a post-hoc test

?TukeyHSD
?
tukey.two.way<-TukeyHSD(two.way)

tukey.two.way


#plot results in a graph

tukey.plot.aov <- aov(mean_TOT ~ wtr_yr + Month, data = dayflow_WY)
tukey.plot.test<-TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test, las = 1)

#===============================================Analysis of exports

one.way <- aov(mean_EXPORT ~ wtr_yr, data = dayflow_WY)

summary(one.way)

two.way <- aov(mean_EXPORT ~ wtr_yr + Month, data = dayflow_WY)

summary(two.way)

#Metric ~ factor(year) + month + region

#test for interaction

two.way.inter <- aov(mean_EXPORT ~ wtr_yr * Month, data = dayflow_WY)

summary(two.way.inter)

#find best fit model - model with lowest AIC score is the best fit for the data

model.set <- list(one.way, two.way, two.way.inter)
model.names <- c("one.way", "two.way", "two.way.inter")

aictab(model.set, modnames = model.names)

#check for homoscedasticity diagnostic plot show unexplained variance (residuals) across the range of observed data. The red line represents mean of residuals and it should be horiz. and centered at zero (or on 1 at the scale-location plot) The normal Q-Q plot plots a regression between the theoretical residuals of a perfectly-homoscedastic model and the actual residuals of your model, so the closer to a slope of 1 this is the better. These diagnostic plots do not show the model fits the assumption of homoscedasticity. I should try the Kruskall-Wallis test

par(mfrow=c(2,2))
plot(two.way)
par(mfrow=c(1,1))

#next do a post-hoc test

?TukeyHSD
?
  tukey.two.way<-TukeyHSD(two.way)

tukey.two.way


#plot results in a graph

tukey.plot.aov <- aov(mean_TOT ~ wtr_yr + Month, data = dayflow_WY)
tukey.plot.test<-TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test, las = 1)

#===============================================Analysis of X2

one.way <- aov(mean_X2 ~ wtr_yr, data = dayflow_WY)

summary(one.way)

two.way <- aov(mean_X2 ~ wtr_yr + Month, data = dayflow_WY)

summary(two.way)

#Metric ~ factor(year) + month + region

#test for interaction

two.way.inter <- aov(mean_X2 ~ wtr_yr * Month, data = dayflow_WY)

summary(two.way.inter)

#find best fit model - model with lowest AIC score is the best fit for the data

model.set <- list(one.way, two.way, two.way.inter)
model.names <- c("one.way", "two.way", "two.way.inter")

aictab(model.set, modnames = model.names)

#check for homoscedasticity diagnostic plot show unexplained variance (residuals) across the range of observed data. The red line represents mean of residuals and it should be horiz. and centered at zero (or on 1 at the scale-location plot) The normal Q-Q plot plots a regression between the theoretical residuals of a perfectly-homoscedastic model and the actual residuals of your model, so the closer to a slope of 1 this is the better. These diagnostic plots do not show the model fits the assumption of homoscedasticity. I should try the Kruskall-Wallis test

par(mfrow=c(2,2))
plot(two.way)
par(mfrow=c(1,1))

#next do a post-hoc test

?TukeyHSD
?aov
two.way <- aov(formula = mean_X2 ~ wtr_yr + Month, data = dayflow_WY)

plot(TukeyHSD(two.way))

tukey.two.way<-TukeyHSD(two.way, "Month")

tukey.two.way


#plot results in a graph

tukey.plot.aov <- aov(mean_TOT ~ wtr_yr + Month, data = dayflow_WY)
tukey.plot.test<-TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test, las = 1)

#plots=======================================================================

# plot the data using ggplot2
ggplot(data=dayflow_month, aes(x = Date, y = mean_TOT)) +
  geom_point() +
  labs(title = "Total Infolow",
       x = "Date",
       y = "mean Total (cfs)")

# Total inflow bargraph using ggplot2

ggplot(data=dayflow_month, aes(x = Date, y = mean_TOT)) +
  geom_bar(stat = "identity", fill = "darkorchid4") +
       xlab("Date")+ ylab("mean Total (cfs)")+
         ggtitle("Total inflow (cfs") +
         theme_bw()

# Total outflow bargraph using ggplot2

ggplot(data=dayflow_month, aes(x = Date, y = mean_OUT)) +
  geom_bar(stat = "identity", fill = "darkorchid4") +
  xlab("Date")+ ylab("mean Outflow (cfs)")+
  ggtitle("Total Outflow (cfs") +
  theme_bw()

# Total inflow bargraph by year

dayflow_month %>%
  ggplot(aes(x = Month, y = mean_TOT)) +
  geom_bar(stat = "identity", fill = "darkorchid4") +
  facet_wrap(~ Year, ncol = 3) +
  labs(title = "Total Monthly inflow",
       subtitle = "Data plotted by year",
       y = "mean Total (cfs)",
       x = "Month") + theme_bw(base_size = 15)

# Total outflow bargraph by year

dayflow_month %>%
  ggplot(aes(x = Month, y = mean_OUT)) +
  geom_bar(stat = "identity", fill = "darkorchid4") +
  facet_wrap(~ Year, ncol = 3) +
  labs(title = "Total Monthly outflow",
       subtitle = "Data plotted by year",
       y = "mean outflow (cfs)",
       x = "Month") + theme_bw(base_size = 15)

# bargraph 2 by year - trying to display months on x axis

dayflow_month %>%
  mutate(month2 = as.Date(paste0("2010-", Month,"-01"),"%Y-%m-%d")) %>%
  ggplot(aes(x = Month, y = mean_TOT)) +
  geom_bar(stat = "identity", fill = "darkorchid4") +
  facet_wrap(~ Year, ncol = 3) +
  labs(title = "Total Monthly inflow",
       subtitle = "Data plotted by year",
       y = "mean Total (cfs)",
       x = "Month") + theme_bw(base_size = 15) +
scale_x_date(date_labels = "%b")

#====================add water year column (oct - sept) using swmrBase

dayflow_4 <- cbind(dayflow_3, waterYear(dayflow_3$Date, numeric=FALSE))

#consider log transforming data, then running ANOVA

dayflow_log <- log(dayflow)

names(dayflow_log) <- paste0(names(dayflow), "_log")

# Create a new data frame with the newly formatted date field and log transform variables and calc monthly mean - quick check I am log transforming before taking mean

dayflow_month_log <- dayflow %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  group_by(Date=floor_date(Date, "month")) %>%
  summarise(Year = mean(Year), 
            Month = mean(Month),
            mean_log_TOT = mean(log(TOT)),
            mean_log_OUT = mean(log(OUT)),
            mean_log_EXPORT = mean(log(EXPORTS)), 
            mean_log_X2 = mean(log(X2)))

#subset time period

dayflow_month_log <- dayflow_month_log %>%
  filter(Date >= as.Date('2010-12-01') & Date <= as.Date('2020-09-01'))

#add water year column (Dec - Nov)

wtr_yr <- function(dates, start_month=12) {
  # Convert dates into POSIXlt
  dates.posix = as.POSIXlt(dates)
  # Year offset
  offset = ifelse(dates.posix$mon >= start_month - 1, 1, 0)
  # Water year
  adj.year = dates.posix$year + 1900 + offset
  # Return the water year
  adj.year
}

#log transform check

dayflow[,c(11,16,24)] <- log(dayflow[,c(11,16,24)])

#compare output below with dayflow_month_log - same when I checked 11/11/21

dayflow_month_log_test_2 <- dayflow %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  group_by(Date=floor_date(Date, "month")) %>% 
  summarise(Year = mean(Year), 
            Month = mean(Month),
            mean_log_TOT = mean(TOT),
            mean_log_OUT = mean(OUT),
            mean_log_EXPORT = mean(EXPORTS), 
            mean_X2 = mean(X2)) 

#subset time period

dayflow_month_log_test_2 <- dayflow_month_log_test_2 %>%
  filter(Date >= as.Date('2010-12-01') & Date <= as.Date('2020-09-01'))
  

dayflow_log_WY <- dayflow_month_log %>% 
  mutate(wtr_yr = wtr_yr(Date))

#===============================================Analysis of log inflow

one.way <- aov(mean_log_TOT ~ wtr_yr, data = dayflow_log_WY)

summary(one.way)

two.way <- aov(mean_log_TOT ~ wtr_yr + Month, data = dayflow_log_WY)

summary(two.way)

#Metric ~ factor(year) + month + region

#test for interaction

two.way.inter <- aov(mean_log_TOT ~ wtr_yr * Month, data = dayflow_log_WY)

summary(two.way.inter)

#find best fit model - model with lowest AIC score is the best fit for the data

model.set <- list(one.way, two.way, two.way.inter)
model.names <- c("one.way", "two.way", "two.way.inter")

aictab(model.set, modnames = model.names)

#check for homoscedasticity diagnostic plot show unexplained variance (residuals) across the range of observed data. The red line represents mean of residuals and it should be horiz. and centered at zero (or on 1 at the scale-location plot) The normal Q-Q plot plots a regression between the theoretical residuals of a perfectly-homoscedastic model and the actual residuals of your model, so the closer to a slope of 1 this is the better. These diagnostic plots do not show the model fits the assumption of homoscedasticity. I should try the Kruskall-Wallis test

par(mfrow=c(2,2))
plot(two.way)
par(mfrow=c(1,1))

#next do a post-hoc test

?TukeyHSD

tukey.two.way<-TukeyHSD(two.way)

tukey.two.way


#plot results in a graph

tukey.plot.aov <- aov(mean_TOT ~ wtr_yr + Month, data = dayflow_WY)
tukey.plot.test<-TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test, las = 1)

#===============================================Analysis of log outflow

one.way <- aov(mean_log_OUT ~ wtr_yr, data = dayflow_log_WY)

summary(one.way)

two.way <- aov(mean_log_OUT ~ wtr_yr + Month, data = dayflow_log_WY)

summary(two.way)

#Metric ~ factor(year) + month + region

#test for interaction

two.way.inter <- aov(mean_log_OUT ~ wtr_yr * Month, data = dayflow_log_WY)

summary(two.way.inter)

#find best fit model - model with lowest AIC score is the best fit for the data

model.set <- list(one.way, two.way, two.way.inter)
model.names <- c("one.way", "two.way", "two.way.inter")

aictab(model.set, modnames = model.names)

#check for homoscedasticity diagnostic plot show unexplained variance (residuals) across the range of observed data. The red line represents mean of residuals and it should be horiz. and centered at zero (or on 1 at the scale-location plot) The normal Q-Q plot plots a regression between the theoretical residuals of a perfectly-homoscedastic model and the actual residuals of your model, so the closer to a slope of 1 this is the better. These diagnostic plots do not show the model fits the assumption of homoscedasticity. I should try the Kruskall-Wallis test

par(mfrow=c(2,2))
plot(two.way)
par(mfrow=c(1,1))

#next do a post-hoc test

#===============================================Analysis of log export

one.way <- aov(mean_log_EXPORT ~ wtr_yr, data = dayflow_log_WY)

summary(one.way)

two.way <- aov(mean_log_EXPORT ~ wtr_yr + Month, data = dayflow_log_WY)

summary(two.way)

#Metric ~ factor(year) + month + region

#test for interaction

two.way.inter <- aov(mean_log_EXPORT ~ wtr_yr * Month, data = dayflow_log_WY)

summary(two.way.inter)

#find best fit model - model with lowest AIC score is the best fit for the data

model.set <- list(one.way, two.way, two.way.inter)
model.names <- c("one.way", "two.way", "two.way.inter")

aictab(model.set, modnames = model.names)

#check for homoscedasticity diagnostic plot show unexplained variance (residuals) across the range of observed data. The red line represents mean of residuals and it should be horiz. and centered at zero (or on 1 at the scale-location plot) The normal Q-Q plot plots a regression between the theoretical residuals of a perfectly-homoscedastic model and the actual residuals of your model, so the closer to a slope of 1 this is the better. These diagnostic plots do not show the model fits the assumption of homoscedasticity. I should try the Kruskall-Wallis test

par(mfrow=c(2,2))
plot(two.way)
par(mfrow=c(1,1))

#next do a post-hoc test

#===============================================Analysis of log X2

one.way <- aov(mean_log_EXPORT ~ wtr_yr, data = dayflow_log_WY)

summary(one.way)

two.way <- aov(mean_log_EXPORT ~ wtr_yr + Month, data = dayflow_log_WY)

summary(two.way)

#Metric ~ factor(year) + month + region

#test for interaction

two.way.inter <- aov(mean_log_EXPORT ~ wtr_yr * Month, data = dayflow_log_WY)

summary(two.way.inter)

#find best fit model - model with lowest AIC score is the best fit for the data

model.set <- list(one.way, two.way, two.way.inter)
model.names <- c("one.way", "two.way", "two.way.inter")

aictab(model.set, modnames = model.names)

#check for homoscedasticity diagnostic plot show unexplained variance (residuals) across the range of observed data. The red line represents mean of residuals and it should be horiz. and centered at zero (or on 1 at the scale-location plot) The normal Q-Q plot plots a regression between the theoretical residuals of a perfectly-homoscedastic model and the actual residuals of your model, so the closer to a slope of 1 this is the better. These diagnostic plots do not show the model fits the assumption of homoscedasticity. I should try the Kruskall-Wallis test

par(mfrow=c(2,2))
plot(two.way)
par(mfrow=c(1,1))

#next do a post-hoc test
