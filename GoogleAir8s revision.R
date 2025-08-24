# install packages #
# load packages #
#=========================================================================
# 202408
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(zoo)
### open the new saved file
# revised data8s = precessed data = data
setwd("~/Desktop/TwinAIR/Google AirView_Dublin/AirViewdata")
data8s = read.csv(file = "Data8sMedian_20230830r1.csv")
View(data8s)
summary(data8s)
dim(data8s)
df_r <- data.frame(data8s)
df_r

## new data file 
## without blank 
## month and days were lubridate
# visualization
# weekday variation by month
library(tidyverse)
library(lubridate)
library(ggplot2)

#time series
alldata_r <-as.POSIXct(format(as.POSIXct(df_r$timestamp_new), tz = "UTC"))
dateonly <-as.Date(alldata_r)
min(dateonly)

# average the second data to hourly concentration
# creat date
# intersect, union
# load packages
# moving average
library(tidyquant)
df_r <- df_r %>% 
  mutate(Date = as.Date(timestamp_new))
write.csv(df_r, file = "df_r1.csv")

# hourly average
# load package
library(dplyr)
# hourly PM2.5
hMedianPM2.5 <- df_r %>% group_by(Date, datetime_h) %>%
  summarise(Median = median(pm25_8sMedian))
hMedianPM2.5

# hourly NO2
hMedianNO2 <- df_r %>% group_by(Date, datetime_h) %>%
  summarise(Median = median(NO2_8s))
hMedianNO2

# Hourly Max O3
hMaxO3 <- df_r %>% group_by(Date, datetime_h) %>%
  summarise(Max = max(O3_8s))
hMaxO3

# hourly CO
hMedianCO <- df_r %>% group_by(Date, datetime_h) %>%
  summarise(Median = median(CO_8s))
hMedianCO

# hourly CO2
hMedianCO2 <- df_r %>% group_by(Date, datetime_h) %>%
  summarise(Median = mean(co2_ppm_8sMedian))
hMedianCO2

# creat date
# hourly average
# intersect, union
## hourly variation PM2.5
library(ggplot2)
hPM2.5 <- ggplot(hMedianPM2.5, aes(x = datetime_h, y = Median)) + 
  geom_boxplot(aes(group = datetime_h), color = "darksalmon", fill = "salmon", alpha = 0.4) + 
  labs(y = expression(PM[2.5]~concentration~(ug/m^3))) +
  scale_x_continuous(expand = c(0,0), name = "Hour", limits = c(6, 19), breaks = seq(6, 19, 1)) +
  scale_y_continuous(limits = c(0,35), breaks = seq(0, 35, 5)) + 
  theme(panel.grid.minor = element_blank())
hPM2.5

## hourly variation NO2
# (NO+NO2)*1.9125
hNO2 <- ggplot(hMedianNO2, aes(x = datetime_h, y = Median)) + 
  geom_boxplot(aes(group = datetime_h), color = "darkseagreen3", fill = "green3", alpha = 0.4) + 
  labs(y = expression(NO[2]~concentration~(ug/m^3))) +
  scale_x_continuous(expand = c(0,0), name = "Hour", limits = c(6, 19), breaks = seq(6, 19, 1)) +
  scale_y_continuous(limits = c(0,100), breaks = seq(0, 100, 20)) + 
  theme(panel.grid.minor = element_blank())
hNO2

#  hourly variation O3
# convert unit
# O3 ppb to ug/m3, 1ppb = 1.9957 ug/m3
hO3 <- ggplot(hMaxO3, aes(x = datetime_h, y = Max)) + 
  geom_boxplot(aes(group = datetime_h), color = "steelblue2", fill = "steelblue", alpha = 0.4) + 
  labs(y = expression(O[3]~concentration~(ug/m^3)) (1 h maximum)) +  
  scale_x_continuous(expand = c(0,0), name = "Hour", limits = c(6, 19), breaks = seq(6, 19, 1)) +
  scale_y_continuous(limits = c(0,120), breaks = seq(0, 120, 20)) + 
  theme(panel.grid.minor = element_blank()) + 
hO3

# hourly variation CO
hCO<- ggplot(hMedianCO, aes(x = datetime_h, y = Median)) + 
  geom_boxplot(aes(group = datetime_h), color = "gold3", fill = "gold1", alpha = 0.4) + 
  scale_x_continuous(name = "Hour", expand = c(0,0), limits = c(6, 19), breaks = seq(6, 19, 1)) +
  scale_y_continuous(name = "CO concentration (ppm)", limits = c(0,1), breaks = seq(0, 1, 0.2)) + 
  theme(panel.grid.minor = element_blank()) + 
  labs("CO")
hCO

# hourly variation CO2
hCO2 <- ggplot(hMedianCO2, aes(x = datetime_h, y = Median)) + 
  geom_boxplot(aes(group = datetime_h), color = "darkslategray4",fill = "darkslategray3", alpha = 0.4) + 
  labs(y = expression(CO[2]~concentration~(ppm))) +
  scale_x_continuous(name = "Hour", expand = c(0,0), limits = c(6, 19), breaks = seq(6, 19, 1)) +
  scale_y_continuous(limits = c(400,500), breaks = seq(0, 500, 20)) + 
  theme(panel.grid.minor = element_blank()) + 
  labs("CO2")
hCO2

# ggplot2 heatmap
# tutorial: https://datascienceplus.com/building-heatmaps-in-r/
##### Month
# daytime PM2.5
# day 
# load csv file
setwd("~/Desktop/TwinAIR/Google AirView_Dublin/AirViewdata")
data8s = read.csv(file = "Data8sMedian_20230830r1.csv")
names(data8s)
View(data8s)
df <- data.frame(data8s)
# creat datetime
df <- df %>% 
  mutate(Date = as.Date(timestamp_new)) #library(dplyr)
View(df)
library(tidyquant)
df <- df %>% 
  mutate(Date = as.Date(timestamp_new))
##### Month
# daytime PM2.5
mMedianPM2.5 <- df %>% group_by(date_m, datetime_h) %>%
  summarise(Median = median(pm25_8sMedian))
mMedianPM2.5

# daytime NO2
mMedianNO2 <- df %>% group_by(date_m, datetime_h) %>%
  summarise(Median = median(NO2_8s))
mMedianNO2

# daytime Max O3
mMaxO3 <- df %>% group_by(date_m, datetime_h) %>%
  summarise(Max = max(O3_8s))
mMaxO3

# daytime CO
mMedianCO <- df %>% group_by(date_m, datetime_h) %>%
  summarise(Median = median(CO_8s))
mMedianCO

# daytime CO2
mMedianCO2 <- df %>% group_by(date_m, datetime_h) %>%
  summarise(Median = mean(co2_ppm_8sMedian))
mMedianCO2

# plot
# revious example
#mNO2 <- ggplot(mMedianNO2, aes(x = date_m, y = datetime_h)) + 
#  geom_tile(aes(fill = Median)) + 
#  scale_fill_gradient(name = 'NO2', low = "#f4fff6", high = "#0bb730") + 
#  scale_y_reverse(name = "Hour of Day", breaks = seq(7, 19, by = 1)) + 
#  scale_x_discrete(expand = c(0,0), position = "bottom", 
#                   limits = c("Jan", "Feb", "Mar", "Apr","May","Jun", 
#                                                          "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), 
#                   labels = c("Jan"="January", "Feb"="February", "Mar"="March","Apr"="April","May"="May", "Jun" = "June",
#                              "Jul" = "July", "Aug" = "August", "Sep" = "September", "Oct" = "October", "Nov" = "November", "Dec" = "December")) + 
#  labs(x = "Year of month")
# mNO2
# pm2.5
mPM2.5 <- ggplot(mMedianPM2.5, aes(x = date_m, y = datetime_h)) + 
  geom_tile(aes(fill = Median)) + 
  scale_fill_gradient(name = expression(PM[2.5]), low = "#f4fff6", high = "#0bb730") + 
  scale_y_reverse(name = "Hour", breaks = seq(7, 19, by = 1)) + 
  scale_x_discrete(expand = c(0,0), position = "bottom", 
                   limits = c("Jan", "Feb", "Mar", "Apr","May","Jun", 
                              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), 
                   labels = c("Jan"="January", "Feb"="February", "Mar"="March","Apr"="April","May"="May", "Jun" = "June",
                              "Jul" = "July", "Aug" = "August", "Sep" = "September", "Oct" = "October", "Nov" = "November", "Dec" = "December")) + 
  labs(x = "Month")
mPM2.5
# no2
mNO2 <- ggplot(mMedianNO2, aes(x = date_m, y = datetime_h)) + 
  geom_tile(aes(fill = Median)) + 
  scale_fill_gradient(name = expression(NO[2]), low = "#f4fff6", high = "#0bb730") + 
  scale_y_reverse(name = "Hour", breaks = seq(7, 19, by = 1)) + 
  scale_x_discrete(expand = c(0,0), position = "bottom", 
                   limits = c("Jan", "Feb", "Mar", "Apr","May","Jun", 
                              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), 
                   labels = c("Jan"="January", "Feb"="February", "Mar"="March","Apr"="April","May"="May", "Jun" = "June",
                              "Jul" = "July", "Aug" = "August", "Sep" = "September", "Oct" = "October", "Nov" = "November", "Dec" = "December")) + 
  labs(x = "Month")
mNO2

#O3
mO3 <- ggplot(mMaxO3, aes(x = date_m, y = datetime_h)) + 
  geom_tile(aes(fill = Max)) + 
  scale_fill_gradient(name = expression(O[3]), low = "#f4fff6", high = "#0bb730") + 
  scale_y_reverse(name = "Hour", breaks = seq(7, 19, by = 1)) + 
  scale_x_discrete(expand = c(0,0), position = "bottom", 
                   limits = c("Jan", "Feb", "Mar", "Apr","May","Jun", 
                              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), 
                   labels = c("Jan"="January", "Feb"="February", "Mar"="March","Apr"="April","May"="May", "Jun" = "June",
                              "Jul" = "July", "Aug" = "August", "Sep" = "September", "Oct" = "October", "Nov" = "November", "Dec" = "December")) + 
  labs(x = "Month")
mO3

mCO <- ggplot(mMedianCO, aes(x = date_m, y = datetime_h)) + 
  geom_tile(aes(fill = Median)) + 
  scale_fill_gradient(name = 'CO', low = "#f4fff6", high = "#0bb730") + 
  scale_y_reverse(name = "Hour", breaks = seq(7, 19, by = 1)) + 
  scale_x_discrete(expand = c(0,0), position = "bottom", 
                   limits = c("Jan", "Feb", "Mar", "Apr","May","Jun", 
                              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), 
                   labels = c("Jan"="January", "Feb"="February", "Mar"="March","Apr"="April","May"="May", "Jun" = "June",
                              "Jul" = "July", "Aug" = "August", "Sep" = "September", "Oct" = "October", "Nov" = "November", "Dec" = "December")) + 
  labs(x = "Month")
mCO

mCO2 <- ggplot(mMedianCO2, aes(x = date_m, y = datetime_h)) + 
  geom_tile(aes(fill = Median)) + 
  scale_fill_gradient(name = expression(CO[2]), low = "#f4fff6", high = "#0bb730") + 
  scale_y_reverse(name = "Hour", breaks = seq(7, 19, by = 1)) + 
  scale_x_discrete(expand = c(0,0), position = "bottom", 
                   limits = c("Jan", "Feb", "Mar", "Apr","May","Jun", 
                              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), 
                   labels = c("Jan"="January", "Feb"="February", "Mar"="March","Apr"="April","May"="May", "Jun" = "June",
                              "Jul" = "July", "Aug" = "August", "Sep" = "September", "Oct" = "October", "Nov" = "November", "Dec" = "December")) + 
  labs(x = "Month")
mCO2

# plot combined figure
library(ggpubr)
jpeg("dirunal and variation across months_r1.jpg", width = 8, height = 15, units = 'in', res = 600)
ggarrange(mPM2.5, mNO2, mO3, mCO, mCO2, 
          labels = c("a", "b", "c", "d", "e", "f"),
          ncol = 1,
          nrow = 5)
dev.off() 

# plot combined figure month and dirunal
library(ggpubr)
jpeg("dirunal and month variation_r1.jpg", width = 17, height = 18, units = 'in', res = 600)
ggarrange(hPM2.5, mPM2.5, hNO2, mNO2,  
          hO3, mO3, hCO, mCO, hCO2, mCO2, 
          labels = c("a", " ", "b", " ", 
                     "c", " ", "d", " ", 
                     "e", " "),
          ncol = 2,
          nrow = 5)
dev.off() 
# 202408
#=======================================================================================================


library(dplyr)
setwd("~/Desktop/TwinAIR/Google AirView_Dublin/AirViewdata")
data8s = read.csv(file = "Data8sMedian_20230830r1.csv")
View(data8s)
summary(data8s)
df_r <- data.frame(data8s)
df_r
summary(df_r)
#select certain rows
#add date
df_r1 <- df_r %>% 
  mutate(Date = as.Date(timestamp_new))
df_r1
  
# creat date

# average second data to hourly
# load packages
library(tidyverse)
library(lubridate)
# moving average
library(ggplot2)
library(dplyr)
library(tidyquant)

# plot figure with moving average
# add as.Date
# hourly PM2.5
hMedianPM2.5 <- df_r %>% group_by(Date, datetime_h) %>%
  summarise(Median = median(pm25_8sMedian))
hMedianPM2.5
# hourly NO2
hMedianNO2 <- df_r %>% group_by(Date, datetime_h) %>%
  summarise(Median = median(NO2_8s))
hMedianNO2

# daily NO2
dMedianNO2 <- df_r %>% group_by(Date, datetime_d) %>%
  summarise(Median = median(NO2_8s))
dMedianNO2


# Hourly Max O3
hMaxO3 <- df_r %>% group_by(Date, datetime_h) %>%
  summarise(Max = max(O3_8s))
hMaxO3
# Hourly Median O3
# hMedianO3 <- df_r %>% group_by(Date, datetime_h) %>%
# summarise(Median = median(O3_8s))
# hMedianO3
# hourly CO
hMedianCO <- df_r %>% group_by(Date, datetime_h) %>%
  summarise(Median = median(CO_8s))
hMedianCO
# hourly CO2
hMedianCO2 <- df_r %>% group_by(Date, datetime_h) %>%
  summarise(Median = median(co2_ppm_8sMedian))
hMedianCO2

# PM2.5
jpeg("moving and smoothing pm2.5.jpg", width = 8, height = 5, units = 'in', res = 500)
mvPM2.5 <- ggplot(hMedianPM2.5, aes(x = Date, y = Median)) + 
  geom_point(aes(color = "Hourly variation")) +        #color = "#E69F45"
  geom_ma(aes(color = "5-day moving average"), ma_fun = WMA, n = 55, size = 2) +          #color = "#276DC2"
  geom_smooth(aes(color = "Smoothing (gam) with 95% confidence interval")) +    #color = "#09557f"
  scale_color_manual(name = "Pollution variation",
                     labels = c("5-day moving average", "Hourly variation", "Smoothing (gam) with 95% confidence interval"), 
                     values = c("Hourly variation" = "gray", "5-day moving average" = "red", "Smoothing (gam) with 95% confidence interval" = "#E69F00")) + 
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90)) + 
  scale_x_date(name = "Date", date_breaks = "1 month", date_labels = "%b %Y", position = "bottom") +
  labs(title = expression(PM[2.5]), y = expression(PM[2.5]~concentration~(ug/m^3))) + 
  geom_hline(yintercept = c(5, 15), col = "black", linetype = "dashed", size = 0.5) +   # WHO Air quality guideline level_Annual  # WHO Air quality guideline level_24-hour
  annotate("rect", xmin = as.Date("2022-01-15"), xmax = as.Date("2022-02-01"), ymin = 0, ymax = 30, alpha = 0.1, fill = "salmon") 
mvPM2.5
dev.off() 

# moving average NO2
jpeg("moving and smoothing no2.jpg", width = 8, height = 5, units = 'in', res = 500)
mvNO2 <- ggplot(hMedianNO2, aes(x = Date, y = Median)) + 
  geom_point(aes(color = "Hourly variation"), size = 1) +        #color = "#E69F45"
  geom_ma(aes(color = "5-day moving average"), ma_fun = WMA, n = 55, size = 2) +          #color = "#276DC2"
  geom_smooth(aes(color = "Smoothing (gam) with 95% confidence interval")) +    #color = "#09557f"
  scale_color_manual(name = "Pollution variation",
                     labels = c("5-day moving average", "Hourly variation", "Smoothing (gam) with 95% confidence interval"), 
                     values = c("Hourly variation" = "gray", "5-day moving average" = "red", "Smoothing (gam) with 95% confidence interval" = "darkseagreen3")) + 
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90)) + 
  scale_x_date(name = "Date", date_breaks = "1 month", date_labels = "%b %Y", position = "bottom") +
  labs(title = expression(NO[2]), y = expression(NO[2]~concentration~(ug/m^3))) + 
  geom_hline(yintercept = c(25, 200), col = "black", linetype = "dashed", size = 0.5) +   # WHO Air quality guideline level_Annual  # WHO Air quality guideline level_24-hour
  annotate("rect", xmin = as.Date("2022-01-04"), xmax = as.Date("2022-02-17"), ymin = 0, ymax = 100, alpha = 0.2, fill = "palegreen3") 
mvNO2
dev.off() 

# moving average day NO2
jpeg("day moving and smoothing no2.jpg", width = 8, height = 5, units = 'in', res = 500)
dmvNO2 <- ggplot(dMedianNO2, aes(x = Date, y = Median)) + 
  geom_point(aes(color = "Daytime median"), size = 1) +        #color = "#E69F45"
  geom_ma(aes(color = "5-day moving average"), ma_fun = WMA, n = 5, size = 2) +          #color = "#276DC2"
  geom_smooth(aes(color = "Smoothing (loess) with 95% confidence interval")) +    #color = "#09557f"
  scale_color_manual(name = "Pollution variation",
                     labels = c("5-day moving average", "Daytime median", "Smoothing (loess) with 95% confidence interval"), 
                     values = c("Daytime median" = "gray", "5-day moving average" = "red", "Smoothing (loess) with 95% confidence interval" = "darkseagreen3")) + 
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90)) + 
  scale_x_date(name = "Date", date_breaks = "1 month", date_labels = "%b %Y", position = "bottom") +
  labs(title = expression(NO[2]), y = expression(NO[2]~concentration~(ug/m^3))) + 
  geom_hline(yintercept = c(10, 25), col = "black", linetype = "dashed", size = 0.5) +   # WHO Air quality guideline level_Annual  # WHO Air quality guideline level_24-hour
  annotate("rect", xmin = as.Date("2022-01-04"), xmax = as.Date("2022-02-17"), ymin = 0, ymax = 100, alpha = 0.2, fill = "palegreen3") 
dmvNO2
dev.off() 

# moving average O3
jpeg("moving and smoothing o3.jpg", width = 8, height = 5, units = 'in', res = 500)
mvO3 <- ggplot(hMaxO3, aes(x = Date, y = Max)) + 
  geom_point(aes(color = "Hourly variation"), size = 1) +        #color = "#E69F45"
  geom_ma(aes(color = "5-day moving average"), ma_fun = WMA, n = 55, size = 2) +          #color = "#276DC2"
  geom_smooth(aes(color = "Smoothing (gam) with 95% confidence interval")) +    #color = "#09557f"
  scale_color_manual(name = "Pollution variation",
                     labels = c("5-day moving average", "Hourly variation", "Smoothing (gam) with 95% confidence interval"), 
                     values = c("Hourly variation" = "gray", "5-day moving average" = "red", "Smoothing (gam) with 95% confidence interval" = "steelblue2")) + 
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90)) + 
  scale_x_date(name = "Date", date_breaks = "1 month", date_labels = "%b %Y", position = "bottom") +
  labs(title = expression(O[3]), y = expression(O[3]~concentration~(ug/m^3))) + 
  geom_hline(yintercept = c(100, 120), col = "black", linetype = "dashed", size = 0.5)    # WHO Air quality guideline level_Annual  # WHO Air quality guideline level_24-hour
mvO3
dev.off() 

# moving average CO
jpeg("moving and smoothing CO.jpg", width = 8, height = 5, units = 'in', res = 500)
mvCO <- ggplot(hMedianCO, aes(x = Date, y = Median)) + 
  geom_point(aes(color = "Hourly variation"), size = 1) +        #color = "#E69F45"
  geom_ma(aes(color = "5-day moving average"), ma_fun = WMA, n = 55, size = 2) +          #color = "#276DC2"
  geom_smooth(aes(color = "Smoothing (gam) with 95% confidence interval")) +    #color = "#09557f"
  scale_color_manual(name = "Pollution variation",
                     labels = c("5-day moving average", "Hourly variation", "Smoothing (gam) with 95% confidence interval"), 
                     values = c("Hourly variation" = "gray", "5-day moving average" = "red", "Smoothing (gam) with 95% confidence interval" = "gold1")) + 
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90)) + 
  scale_x_date(name = "Date", date_breaks = "1 month", date_labels = "%b %Y", position = "bottom") +
  scale_y_continuous(name = "CO concentration (ppm)") + 
  labs(title = "CO")    # WHO Air quality guideline level_Annual  # WHO Air quality guideline level_24-hour
mvCO
dev.off() 

# moving average CO2
jpeg("moving and smoothing CO2.jpg", width = 8, height = 5, units = 'in', res = 500)
mvCO2 <- ggplot(hMedianCO2, aes(x = Date, y = Median)) + 
  geom_point(aes(color = "Hourly variation"), size = 1) +        #color = "#E69F45"
  geom_ma(aes(color = "5-day moving average"), ma_fun = WMA, n = 55, size = 2) +          #color = "#276DC2"
  geom_smooth(aes(color = "Smoothing (gam) with 95% confidence interval")) +    #color = "#09557f"
  scale_color_manual(name = "Pollution variation",
                     labels = c("5-day moving average", "Hourly variation", "Smoothing (gam) with 95% confidence interval"), 
                     values = c("Hourly variation" = "gray", "5-day moving average" = "red", "Smoothing (gam) with 95% confidence interval" = "lightskyblue3")) + 
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90)) + 
  scale_x_date(name = "Date", date_breaks = "1 month", date_labels = "%b %Y", position = "bottom") +
  labs(title = expression(CO[2]), y = expression(CO[2]~concentration~(ppm)))    # WHO Air quality guideline level_Annual  # WHO Air quality guideline level_24-hour
mvCO2
dev.off() 

# plot combined figure
jpeg("5 day moving average and smooting_r1.jpg", width = 16, height = 14, units = 'in', res = 600)
ggarrange(mvPM2.5, mvCO, dmvNO2, mvCO2, mvO3, 
          labels = c("a", "b", "c", "d", "e"),
          ncol = 2,
          nrow = 3)
dev.off() 
# different display of figures
jpeg("5 day moving average and smooting_32.jpg", width = 16, height = 14, units = 'in', res = 600)
ggarrange(mvPM2.5, mvCO, dmvNO2, mvCO2, mvO3,
          labels = c("a", "b", "c", "d", "e"),
          ncol = 2,
          nrow = 3)
dev.off() 


# daily average
# intersect, union
df_r<- df_r %>% 
  mutate(Date = as.Date(timestamp_new))
MdPM2.5 <- df_r %>% group_by(Date, datetime_d) %>%
  summarise(Median = median(pm25_8sMedian))
MdPM2.5    
# creat date
# hourly average
# intersect, union
# plot figure no2
# NOx in ug/m3 is expressed as NO2, (NO+NO2)*1.9125
df_r <- df_r %>% 
  mutate(Date = as.Date(timestamp_new))
df_r<- df_r %>% 
  rowwise() %>%
  mutate(rowsum = sum(c(no_ppb_8sMedian,no2_ppb_8sMedian))*1.9125)
Avgno2 <- df_r %>% group_by(Date, datetime_h) %>%
  summarise(Median = median(rowsum))
Avgno2

# daily NO2
dMedianNO2 <- df_r %>% group_by(Date, datetime_d) %>%
  summarise(Median = median(NO2_8s))
dMedianNO2
# daily CO
dMedianCO <- df_r %>% group_by(Date, datetime_d) %>%
  summarise(Median = median(CO_8s))
dMedianCO


#==================================
# install.packages("wavethresh")
library(wavethresh)
# plot time series
library(dplyr)
library(ggplot2)
library(tidyr)
setwd("~/Desktop/TwinAIR/Google AirView_Dublin/AirViewdata")
data = read.csv(file = "durinal patterns LB.csv")
View(data)
summary(data)
df = data.frame(data)
Date <- as.Date(df$Date)
head(data)
PM2.5
colors <- c("Average.of.NO2_B" = "skyblue", "Average.of.NO2_L" = "#4682B4", "Average.of.PM2.5_B" = "gold", "Average.of.PM2.5_L" = "darkgoldenrod")
jpeg("dirunal distribution.jpg", width = 7, height = 5.5, units = 'in', res = 600)
ggplot(df, aes(x = Hours), color = c("Average.of.NO2_B" = "skyblue", "Average.of.NO2_L" = "#4682B4", "Average.of.PM2.5_B" = "gold", "Average.of.PM2.5_L" = "darkgoldenrod")) +
  geom_area(aes(y = Average.of.NO2_B), color = "skyblue", fill = "skyblue", alpha = 0.5, show.legend = TRUE) + 
  geom_line(aes(y = Average.of.NO2_L), color = "#4682B4", show.legend = TRUE) + 
  geom_area(aes(y = Average.of.PM2.5_B), color = "gold", fill = "gold", alpha = 0.6, show.legend = TRUE) + 
  geom_line(aes(y =  Average.of.PM2.5_L), color = "darkgoldenrod", show.legend = TRUE) + 
  scale_x_continuous(limits = c(6, 19), breaks = seq(6, 19, 1)) + 
  scale_color_manual(values = c("Average.of.NO2_B"= "skyblue", "Average.of.NO2_L"= "#4682B4", "Average.of.PM2.5_B"= "gold", "Average.of.PM2.5_L"= "darkgoldenrod")) + 
  scale_fill_manual(values = c("#4682B4", "", "gold", "")) + 
  guides(fill = guide_legend(reverse = TRUE)) + 
  #  scale_color_manual(
  #                    breaks = c("Average.of.NO2_B", "Average.of.NO2_L", "Average.of.PM2.5_B", "Average.of.PM2.5_L"),
  #                    labels = c(NO2_Background = "skyblue", NO2_Local = "#4682B4", PM2.5_Background = "gold", PM2.5_Local = "darkgoldenrod"),
  #                     values = colors) + 
  theme(legend.position = "bottom") + 
  labs(x = "Hour", y = expression(Concentration~(ug/m^3)), color = "Legend")  
dev.off() 

#==================================
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(zoo)


# Laying out multiple plots on a page: https://cran.r-project.org/web/packages/egg/vignettes/Ecosystem.html
# Creating a multiple area chart https://biostats.w.uib.no/creating-a-multiple-area-chart/
setwd("~/Desktop/TwinAIR/Google AirView_Dublin/AirViewdata")
data = read.csv(file = "hourlycontribution.csv")
View(data)
summary(data)
df <- data.frame(data)
df
# creat datetime
library(dplyr)
df_r <- df %>% 
  mutate(date = as.Date(Date))
View(df_r)
# PM2.5 
jpeg("dirunal distribution.jpg", width = 7, height = 5.5, units = 'in', res = 600)
hpm2.5 <- ggplot(df_r, aes(x = date), color = c("PM2.5_Background..Hourly." = "gold", "PM2.5_Local..Hourly." = "darkgoldenrod")) +
  geom_area(aes(y = PM2.5_Background..Hourly.), color = "gold", fill = "gold", alpha = 0.5, show.legend = TRUE) + 
  geom_area(aes(y = PM2.5_Local..Hourly.), color = "darkgoldenrod", fill = "darkgoldenrod", alpha = 0.6, show.legend = TRUE) + 
  scale_x_date(name = "Date", date_breaks = "1 month", date_labels = "%b-%Y", position = "bottom") +
  scale_fill_manual(values = c("gold", "darkgoldenrod")) + 
  guides(fill = guide_legend(values = c("gold", "darkgoldenrod"))) + 
  scale_color_manual(breaks = c("PM2.5_Background..Hourly.", "PM2.5_Local..Hourly."),
                     labels = c(PM2.5_Background..Hourly. = "gold", PM2.5_Local..Hourly. = "darkgoldenrod")) + 
  theme(legend.position = "bottom") + 
  labs(x = "Date", y = expression(Concentration~(ug/m^3)), color = "Legend", 
       title = expression(Time~series~decomposition~of~short~lived~PM[2.5])) 
hpm2.5
dev.off() 

# NO2
jpeg("dirunal distribution.jpg", width = 7, height = 5.5, units = 'in', res = 600)
hNO2 <- ggplot(df_r, aes(x = date), color = c("NO2_Background..Hourly." = "skyblue", "NO2_Local..Hourly." = "#4682B4")) +
  geom_area(aes(y = NO2_Background..Hourly.), color = "skyblue", fill = "skyblue", alpha = 0.5, show.legend = TRUE) + 
  geom_area(aes(y = NO2_Local..Hourly.), color = "#4682B4", fill = "#4682B4", alpha = 0.6, show.legend = TRUE) + 
  scale_x_date(name = "Date", date_breaks = "1 month", date_labels = "%b-%Y", position = "bottom") +
  scale_fill_manual(values = c("skyblue", "#4682B4")) + 
  guides(fill = guide_legend(values = c("skyblue", "#4682B4"))) + 
  scale_color_manual(breaks = c("PM2.5_Background..Hourly.", "PM2.5_Local..Hourly."),
                     labels = c(PM2.5_Background..Hourly. = "skyblue", PM2.5_Local..Hourly. = "#4682B4")) + 
  theme(legend.position = "bottom") + 
  labs(x = "Date", y = expression(Concentration~(ug/m^3)), color = "Legend", 
       title = expression(Time~series~decomposition~of~short~lived~NO[2])) 
hNO2
dev.off() 

# Creating a multiple area chart https://biostats.w.uib.no/creating-a-multiple-area-chart/
setwd("~/Desktop/TwinAIR/Google AirView_Dublin/AirViewdata")
data = read.csv(file = "daytimecontribution.csv")
View(data)
summary(data)
df <- data.frame(data)
df
# creat datetime
library(dplyr)
df_r1 <- df %>% 
  mutate(date = as.Date(Date))
View(df_r1)

# PM2.5 
jpeg("dirunal distribution.jpg", width = 7, height = 5.5, units = 'in', res = 600)
dpm2.5 <- ggplot(df_r1, aes(x = date), color = c("PM2.5_Background..Daytime." = "gold1", "PM2.5_Local..Daytime." = "goldenrod3")) +
  geom_area(aes(y = PM2.5_Background..Daytime.), color = "gold1", fill = "gold1", alpha = 0.5, show.legend = TRUE) + 
  geom_area(aes(y = PM2.5_Local..Daytime.), color = "goldenrod3", fill = "goldenrod3", alpha = 0.6, show.legend = TRUE) + 
  scale_x_date(name = "Date", date_breaks = "1 month", date_labels = "%b-%Y", position = "bottom") +
  scale_fill_manual(values = c("gold1", "goldenrod3")) + 
  guides(fill = guide_legend(values = c("gold1", "goldenrod3"))) + 
  scale_color_manual(breaks = c("PM2.5_Background..Daytime.", "PM2.5_Local..Daytime."),
                     labels = c(PM2.5_Background..Daytime. = "gold1", PM2.5_Local..Daytime. = "goldenrod3")) + 
  theme(legend.position = "bottom") + 
  labs(x = "Date", y = expression(Concentration~(ug/m^3)), color = "Legend", 
       title = expression(Time~series~decomposition~of~longer~lived~PM[2.5])) 
dpm2.5
dev.off() 

# NO2
jpeg("dirunal distribution.jpg", width = 7, height = 5.5, units = 'in', res = 600)
dNO2 <- ggplot(df_r1, aes(x = date), color = c("NO2_Background..Daytime." = "skyblue1", "NO2_Local..Daytime." = "skyblue4")) +
  geom_area(aes(y = NO2_Background..Daytime.), color = "skyblue1", fill = "skyblue1", alpha = 0.5, show.legend = TRUE) + 
  geom_area(aes(y = NO2_Local..Daytime.), color = "skyblue4", fill = "skyblue4", alpha = 0.6, show.legend = TRUE) + 
  scale_x_date(name = "Date", date_breaks = "1 month", date_labels = "%b-%Y", position = "bottom") +
  scale_fill_manual(values = c("skyblue1", "skyblue4")) + 
  guides(fill = guide_legend(values = c("skyblue1", "skyblue4"))) + 
  scale_color_manual(breaks = c("NO2_Background..Daytime.", "NO2_Local..Daytime."),
                     labels = c(NO2_Background..Daytime. = "skyblue1", NO2_Local..Daytime. = "skyblue4")) + 
  theme(legend.position = "bottom") + 
  labs(x = "Date", y = expression(Concentration~(ug/m^3)), color = "Legend", 
       title = expression(Time~series~decomposition~of~longer~lived~NO[2])) 
dNO2
dev.off() 

# plot combined figure
jpeg("short term contribution of pollution.jpg", width = 14, height = 18, units = 'in', res = 600)
ggarrange(hpm2.5, hNO2, dpm2.5, dNO2,  
          labels = c("a", "b", "c", "d"),
          ncol = 1,
          nrow = 4)
dev.off() 

#==================================
#==================================
# Creating a multiple area chart https://biostats.w.uib.no/creating-a-multiple-area-chart/
# add legend: https://tilburgsciencehub.com/topics/visualization/data-visualization/graphs-charts/time-series-ggplot2/
setwd("~/Desktop/TwinAIR/Google AirView_Dublin/AirViewdata")
data = read.csv(file = "hourlycontributionr.csv")
View(data)
summary(data)
df <- data.frame(data)
df
# creat datetime
library(dplyr)
df_r <- df %>% 
  mutate(date = as.Date(Date))
View(df_r)
# PM2.5 
jpeg("dirunal distribution.jpg", width = 7, height = 5.5, units = 'in', res = 600)
hpm2.5 <- ggplot(df_r, aes(x = date, y = PM2.5, fill = contribution)) +
  geom_area(alpha = 0.5, show.legend = TRUE) + 
  scale_color_manual(values = c("Backround" = "gold", "Local" = "darkgoldenrod")) + 
  scale_fill_manual(values = c("gold", "darkgoldenrod")) + 
  scale_x_date(name = "Date", date_breaks = "1 month", date_labels = "%b-%Y", position = "bottom") + 
  theme(legend.position = "bottom") + 
  labs(x = "Date", y = expression(Concentration~(ug/m^3)), 
       title = expression(Time~series~decomposition~of~short~lived~PM[2.5])) 
hpm2.5
dev.off() 

# NO2
jpeg("dirunal distribution.jpg", width = 7, height = 5.5, units = 'in', res = 600)
hNO2 <- ggplot(df_r, aes(x = date, y = NO2, fill = contribution)) +
  geom_area(alpha = 0.5, show.legend = TRUE) + 
  scale_color_manual(values = c("Background" = "skyblue", "Local" = "#4682B4")) + 
  scale_fill_manual(values = c("skyblue", "#4682B4")) +   
  scale_x_date(name = "Date", date_breaks = "1 month", date_labels = "%b-%Y", position = "bottom") +
  theme(legend.position = "bottom") + 
  labs(x = "Date", y = expression(Concentration~(ug/m^3)),
       title = expression(Time~series~decomposition~of~short~lived~NO[2])) 
hNO2
dev.off() 


# Creating a multiple area chart https://biostats.w.uib.no/creating-a-multiple-area-chart/
setwd("~/Desktop/TwinAIR/Google AirView_Dublin/AirViewdata")
data = read.csv(file = "daytimecontributionr.csv")
View(data)
summary(data)
df <- data.frame(data)
df
# creat datetime
library(dplyr)
df_r1 <- df %>% 
  mutate(date = as.Date(Date))
View(df_r1)

# PM2.5 
jpeg("dirunal distribution.jpg", width = 7, height = 5.5, units = 'in', res = 600)
dpm2.5 <- ggplot(df_r1, aes(x = date, y = PM2.5, fill = contribution, color = contribution)) +
  geom_area(alpha = 0.5, show.legend = TRUE) + 
  scale_color_manual(values = c("Background" = "gold1", "Local" = "goldenrod3")) + 
  scale_fill_manual(values = c("gold1", "goldenrod3")) + 
  scale_x_date(name = "Date", date_breaks = "1 month", date_labels = "%b-%Y", position = "bottom") + 
  theme(legend.position = "bottom") + 
  labs(x = "Date", y = expression(Concentration~(ug/m^3)), 
       title = expression(Time~series~decomposition~of~longer~lived~PM[2.5])) 
dpm2.5
dev.off() 

# NO2
jpeg("dirunal distribution.jpg", width = 7, height = 5.5, units = 'in', res = 600)
dNO2 <- ggplot(df_r1, aes(x = date, y = NO2, fill = contribution, color = contribution)) +
  geom_area(alpha = 0.5, show.legend = TRUE) + 
  scale_color_manual(values = c("Background" = "skyblue1", "Local" = "skyblue4")) + 
  scale_fill_manual(values = c("skyblue1", "skyblue4")) +   
  scale_x_date(name = "Date", date_breaks = "1 month", date_labels = "%b-%Y", position = "bottom") +
  theme(legend.position = "bottom") + 
  labs(x = "Date", y = expression(Concentration~(ug/m^3)),
       title = expression(Time~series~decomposition~of~longer~lived~NO[2])) 
dNO2
dev.off() 

# plot combined figure
jpeg("short term contribution of pollution_r.jpg", width = 14, height = 18, units = 'in', res = 600)
ggarrange(hpm2.5, hNO2, dpm2.5, dNO2,  
          labels = c("a", "b", "c", "d"),
          ncol = 1,
          nrow = 4)
dev.off() 

#corr subscript: https://stackoverflow.com/questions/49067364/r-subscript-in-the-axis-labels-of-a-plotcorr-plot
#==================================
### open the new saved file
# revised data8s = precessed data = data
setwd("~/Desktop/TwinAIR/Google AirView_Dublin/AirViewdata")
data8s = read.csv(file = "Data8sMedian_20230830r1.csv")
View(data8s)
summary(data8s)
dim(data8s)
df_r <- data.frame(data8s)
df_r
library(tidyverse)
library(lubridate)
library(ggplot2)
# monthly median
# first make date
df_r <- df_r %>% 
  mutate('yearmon' = make_date(year = datetime_y, month = datetime_m))
head(df_r)
# lab the month
yearmon <- df_r %>% 
  mutate(yearmon = make_datetime(datetime_y, datetime_m))
head(yearmon)
# PM2.5 bar chart median
mPM2.5 <- df_r %>%
  group_by(yearmon) %>%
  summarize(m = median(pm25_8sMedian)) %>% 
  ggplot(aes(x = yearmon, y = m)) + 
  geom_col(color = "#E69F00", fill = "darksalmon") + 
  scale_x_date(labels = scales::label_date("%b %Y"), breaks = as.Date(c("2021-05-01", "2022-05-06")), date_breaks = "1 month") + 
  scale_y_continuous(limits = c(0,15), breaks = seq(0, 15, 5)) + 
  theme(axis.text.x = element_text(angle = 90))  + 
  labs(x = "Month", y = expression(PM[2.5]~concentration~(ug/m^3)))
mPM2.5

# NO2 bar chart median
mNO2 <- df_r %>%
  group_by(yearmon) %>%
  summarize(m = median(NO2_8s)) %>% 
  ggplot(aes(x = yearmon, y = m)) + 
  geom_col(color = "palegreen3", fill = "darkseagreen3") + 
  scale_x_date(labels = scales::label_date("%b %Y"), breaks = as.Date(c("2021-05-01", "2022-05-06")), date_breaks = "1 month") + 
  scale_y_continuous(limits = c(0,50), breaks = seq(0, 60, 10)) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(x = "Month", y = expression(NO[2]~concentration~(ug/m^3)))
mNO2

# O3 bar chart median
mO3 <- df_r %>%
  group_by(yearmon) %>%
  summarize(m = median(O3_8s)) %>% 
  ggplot(aes(x = yearmon, y = m)) + 
  geom_col(color = "steelblue2", fill = "dodgerblue") + 
  scale_x_date(labels = scales::label_date("%b %Y"), breaks = as.Date(c("2021-05-01", "2022-05-06")), date_breaks = "1 month") + 
  scale_y_continuous() + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(x = "Month", y = expression(O[3]~concentration~(ug/m^3)))
mO3

# O3 bar chart max
maxO3 <- df_r %>%
  group_by(yearmon) %>%
  summarize(m = max(O3_8s)) %>% 
  ggplot(aes(x = yearmon, y = m)) + 
  geom_col(color = "dodgerblue", fill = "steelblue2") + 
  scale_x_date(labels = scales::label_date("%b %Y"), breaks = as.Date(c("2021-05-01", "2022-05-06")), date_breaks = "1 month") + 
  scale_y_continuous() + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(x = "Month", y = expression(O[3]~concentration~(ug/m^3)))
maxO3

# CO2 bar chart
mCO2 <- df_r %>%
  group_by(yearmon) %>%
  summarize(m = median(co2_ppm_8sMedian)) %>% 
  ggplot(aes(x = yearmon, y = m)) + 
  geom_col(color = "lightskyblue3", fill = "darkslategray4") + 
  scale_x_date(labels = scales::label_date("%b %Y"), breaks = as.Date(c("2021-05-01", "2022-05-06")), date_breaks = "1 month") + 
  scale_y_continuous(limits = c(0,500), breaks = seq(0, 500, 100)) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(x = "Month", y = expression(CO[2]~concentration~(ppm)))
mCO2

# CO bar chart median
mCO <- df_r %>%
  group_by(yearmon) %>%
  summarize(m = median(CO_8s)) %>% 
  ggplot(aes(x = yearmon, y = m)) + 
  geom_col(color = "wheat", fill = "gold1") + 
  scale_x_date(labels = scales::label_date("%b %Y"), breaks = as.Date(c("2021-05-01", "2022-05-06")), date_breaks = "1 month") + 
  scale_y_continuous(name = "CO concentration (ppm)", limits = c(0,0.5), breaks = seq(0,0.5, 0.1)) + 
  xlab("Month") + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs("CO")
mCO
# arrange a combined figure
jpeg("Monthly variation_r.jpg", width = 16, height = 10, units = 'in', res = 600)
ggarrange(mPM2.5, mCO, mNO2, mCO2, mO3, maxO3,
          labels = c("a", "b", "c", "d", "e", "f"),
          ncol = 2,
          nrow = 3)
dev.off() 
######

##### 
# hourly correlation
# moving average
setwd("~/Desktop/TwinAIR/Google AirView_Dublin/AirViewdata")
data8s = read.csv(file = "Data8sMedian_20230830r1.csv")
names(data8s)
View(data8s)
df <- data.frame(data8s)
# creat datetime
df <- df %>% 
  mutate(Date = as.Date(timestamp_new)) #library(dplyr)
View(df)

library(lubridate)
library(tidyquant)
library(ggplot2)
library(tidyr)
#correlation
library(corrplot)
library(Hmisc)
library(ggcorrplot) # plot ggcorrplot

# hourly correlation
# moving average
library(tidyquant)
df <- df %>% 
  mutate(Date = as.Date(timestamp_new))

library(dplyr)
# hourly PM2.5
hMedianPM2.5 <- df %>% group_by(Date, datetime_h) %>%
  summarise(Median = median(pm25_8sMedian))
hMedianPM2.5

# hourly NO2
hMedianNO2 <- df %>% group_by(Date, datetime_h) %>%
  summarise(Median = median(NO2_8s))
hMedianNO2

# Hourly Max O3
hMaxO3 <- df %>% group_by(Date, datetime_h) %>%
  summarise(Max = max(O3_8s))
hMaxO3

# Hourly Median O3
hMedianO3 <- df %>% group_by(Date, datetime_h) %>%
  summarise(Median = median(O3_8s))
hMedianO3

# hourly CO
hMedianCO <- df %>% group_by(Date, datetime_h) %>%
  summarise(Median = median(CO_8s))
hMedianCO

# hourly CO2
hMedianCO2 <- df %>% group_by(Date, datetime_h) %>%
  summarise(Median = mean(co2_ppm_8sMedian))
hMedianCO2
# dataframe
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(zoo)
df1 <- data.frame(hMedianPM2.5, hMedianNO2, hMedianO3, hMedianCO, hMedianCO2)
View(df1)
summary(df1)
df2 <- df1[, c("Median", "Median.1", "Median.2", 
               "Median.3", "Median.4")]

colnames(df2) <- c(':PM[2.5]', ':NO[2]', ':O[3]', 'CO', ':CO[2]')
colnames(df2)
View(df2)
asso <- df2[, c(":PM[2.5]", ":NO[2]", ":O[3]", 
                "CO", ":CO[2]")]

M = cor(asso)
corrplot(M, method = "number")
h <- ggcorrplot(M,
                outline.color = "white",
                hc.order = TRUE,
                sig.level = 0.05,
                insig = "blank",
                colors = c("#6D9EC1", "white", "#E46726"),
                lab = TRUE)
h
#title = "Correlation matrix of the monitored air quality features using median hourly concentrations"

# day 
library(tidyquant)
df <- df %>% 
  mutate(Date = as.Date(timestamp_new))
# hourly average
# load package
library(dplyr)
# daytime PM2.5
dMedianPM2.5 <- df %>% group_by(Date, datetime_d) %>%
  summarise(Median = median(pm25_8sMedian))
dMedianPM2.5

# daytime NO2
dMedianNO2 <- df %>% group_by(Date, datetime_d) %>%
  summarise(Median = median(NO2_8s))
dMedianNO2

# daytime Max O3
dMaxO3 <- df %>% group_by(Date, datetime_d) %>%
  summarise(Max = max(O3_8s))
dMaxO3

# daytime Median O3
dMedianO3 <- df %>% group_by(Date, datetime_d) %>%
  summarise(Median = median(O3_8s))
dMedianO3

# daytime CO
dMedianCO <- df %>% group_by(Date, datetime_d) %>%
  summarise(Median = median(CO_8s))
dMedianCO

# daytime CO2
dMedianCO2 <- df %>% group_by(Date, datetime_d) %>%
  summarise(Median = mean(co2_ppm_8sMedian))
dMedianCO2
# dataframe
library(lubridate)
library(zoo)
df1 <- data.frame(dMedianPM2.5, dMedianNO2, dMedianO3, dMedianCO, dMedianCO2)
View(df1)
summary(df1)
df2 <- df1[, c("Median", "Median.1", "Median.2", 
               "Median.3", "Median.4")]
colnames(df2) <- c(':PM[2.5]', ':NO[2]', ':O[3]', 'CO', ':CO[2]')
colnames(df2)
View(df2)
asso <- df2[, c(":PM[2.5]", ":NO[2]", ":O[3]", 
                "CO", ":CO[2]")]

# tutorial: https://towardsdatascience.com/customizable-correlation-plots-in-r-b1d2856a4b05
M1 = cor(asso)
corrplot(M, method = "circle")
d <- ggcorrplot(M1,
                outline.color = "white",
                hc.order = TRUE,
                sig.level = 0.05,
                insig = "blank",
                colors = c("#6D9EC1", "white", "#E46726"),
                lab = TRUE)
#title = "Correlation matrix of the monitored air quality features using median daytime concentrations"

# different display of figures
library(ggpubr) #for ggarrange
jpeg("hd corr.jpg", width = 12, height = 5.5, units = 'in', res = 600)
ggarrange(h, d, 
          labels = c("a. Hourly", "b. Daytime"),
          ncol = 2,
          nrow = 1)
dev.off() 

#===============================
# revised data8s = precessed data = data
setwd("~/Desktop/TwinAIR/Google AirView_Dublin/AirViewdata")
data8s = read.csv(file = "Data8sMedian_20230830r1.csv")
View(data8s)
summary(data8s)
dim(data8s)
df_r <- data.frame(data8s)
df_r
df_r <- df_r %>% 
  mutate('yearmon' = make_date(year = datetime_y, month = datetime_m))
head(df_r)
df_r
View(df_r)
# weekday variation by month
library(tidyverse)
library(lubridate)
library(ggplot2)
# lab the month
yearmon <- df_r %>% 
  mutate(yearmon = make_datetime(datetime_y, datetime_m))
head(yearmon)

# weekday variation
# PM2.5
wPM2.5 <- df_r %>% 
  group_by(wday) %>%
  summarize(m = median(pm25_8sMedian)) %>% 
  ggplot(aes(x = factor(wday, levels = c("Mon", "Tue", "Wed", "Thu", "Fri")), y = m, fill = wday)) + 
  guides(fill = guide_legend("Day of week")) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label = round(m, 2)), color = "black", vjust = 1.6, size = 3.5) + 
  theme(axis.text.x = element_text(angle = 90), legend.position = "none") + 
  labs(x = "Day of week", y = expression(Concentration~(ug/m^3)), title = expression(PM[2.5])) 
wPM2.5

# NO2
wNO2 <- df_r %>% 
  group_by(wday) %>%
  summarize(m = median(NO2_8s)) %>% 
  ggplot(aes(x = factor(wday, levels = c("Mon", "Tue", "Wed", "Thu", "Fri")), y = m, fill = wday)) + 
  guides(fill = guide_legend("Day of week")) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label = round(m, 2)), color = "black", vjust = 1.6, size = 3.5) + 
  theme(axis.text.x = element_text(angle = 90), legend.position = "none") + 
  labs(x = "Day of week", y = expression(Concentration~(ug/m^3)), title = expression(NO[2]))
wNO2

# median O3
wO3 <- df_r %>% 
  group_by(wday) %>%
  summarize(m = median(O3_8s)) %>% 
  ggplot(aes(x = factor(wday, levels = c("Mon", "Tue", "Wed", "Thu", "Fri")), y = m, fill = wday)) + 
  guides(fill = guide_legend("Day of week")) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label = round(m, 1)), color = "black", vjust = 1.6, size = 3.5) + 
  theme(axis.text.x = element_text(angle = 90), legend.position = "none") +    # add legend.position to remove a legend
  labs(x = "Day of week", y = expression(Concentration~(ug/m^3)), title = expression(O[3]))
wO3
# max O3
wmaxO3 <- df_r %>% 
  group_by(wday) %>%
  summarize(m = max(O3_8s)) %>% 
  ggplot(aes(x = factor(wday, levels = c("Mon", "Tue", "Wed", "Thu", "Fri")), y = m, fill = wday)) + 
  guides(fill = guide_legend("Day of week")) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label = round(m, 1)), color = "black", vjust = 1.6, size = 3.5) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(x = "Day of week", y = expression(Concentration~(ug/m^3)), title = expression(O[3]~maximum))
wmaxO3
# CO2
wCO2 <- df_r %>% 
  group_by(wday) %>%
  summarize(m = median(co2_ppm_8sMedian)) %>% 
  ggplot(aes(x = factor(wday, levels = c("Mon", "Tue", "Wed", "Thu", "Fri")), y = m, fill = wday)) + 
  guides(fill = guide_legend("Day of week")) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label = round(m, 1)), color = "black", vjust = 1.6, size = 3.5) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(x = "Day of week", y = expression(Concentration~(ppm)), title = expression(CO[2]))
wCO2
# CO
wCO <- df_r %>% 
  group_by(wday) %>%
  summarize(m = median(CO_8s)) %>% 
  ggplot(aes(x = factor(wday, levels = c("Mon", "Tue", "Wed", "Thu", "Fri")), y = m, fill = wday)) + 
  guides(fill = guide_legend("Day of week")) + 
  geom_bar(stat = "identity") + 
  scale_y_continuous(name = "CO concentration (ppm)") + 
  geom_text(aes(label = round(m, 2)), color = "black", vjust = 1.6, size = 3.5) + 
  xlab("Day of week") + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(title = "CO")
wCO
# plot combined figure
library(ggpubr)
jpeg("Day of week variation_r.jpg", width = 16, height = 10, units = 'in', res = 600)
ggarrange(wPM2.5, wCO, wNO2, wCO2, wO3, wmaxO3,
          labels = c("a", "b", "c", "d", "e", "f"),
          ncol = 2,
          nrow = 3)
dev.off() 

#===============================
#===============================


