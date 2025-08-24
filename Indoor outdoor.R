# install packages #
# load packages #
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(zoo)

# read csv #
# view file #
# indoor air
setwd("~/Desktop/AIVC2023")
indoor = read.csv(file = "16389_1695308738217-indoor.csv")
View(indoor)
indoor <-na.omit(indoor)
dfindoor <- data.frame(indoor)
colnames(dfindoor) <- c('time', 'RH', 'T', 'P', 'Battery','eCO2', 'TVOC', 'light','PM1', 'noise', 'PM10','PM25')
colnames(dfindoor)

# dateonly
indoordata <-as.POSIXct(format(as.POSIXct(dfindoor$time), tz = "UTC"))
dateonly <-as.Date(indoordata)
head(dateonly)

# add date
library(tidyquant)
dfindoor <- dfindoor %>% 
  mutate(Date = as.Date(time))
dfindoor
datetime_h <- hour(ymd_hms(dfindoor$time, tz = "Europe/Dublin"))
dfindoor <- dfindoor %>% 
  mutate(dfindoor, h = datetime_h)
dfindoor
# pm2.5
hPM2.5 <- dfindoor %>% group_by(Date, h) %>%
  summarise(Mean = mean(PM25))
hPM2.5
# pm10
hPM10 <- dfindoor %>% group_by(Date, h) %>%
  summarise(Mean = mean(PM10))
hPM10
# pm1
hPM1 <- dfindoor %>% group_by(Date, h) %>%
  summarise(Mean = mean(PM1))
hPM1
# tep
T <- dfindoor %>% group_by(Date, h) %>%
  summarise(Mean = mean(T))
T
# rh
RH <- dfindoor %>% group_by(Date, h) %>%
  summarise(Mean = mean(RH))
RH
# pressure
P <- dfindoor %>% group_by(Date, h) %>%
  summarise(Mean = mean(P))
P
# eCO2
eCO2 <- dfindoor %>% group_by(Date, h) %>%
  summarise(Mean = mean(eCO2))
eCO2
# TVOC
TVOC <- dfindoor %>% group_by(Date, h) %>%
  summarise(Mean = mean(TVOC))
TVOC
# TVOC
light <- dfindoor %>% group_by(Date, h) %>%
  summarise(Mean = mean(light))
light
# noise
noise <- dfindoor %>% group_by(Date, h) %>%
  summarise(Mean = mean(noise))
noise
# export the dataframe with new columns of seperated date time
library(data.table)
library(tibble)
library(xts)
library(lubridate)
library(tidyverse)
dateset <- list(hPM2.5,hPM10, hPM1, T, RH, P, eCO2, TVOC, light, noise)
write.csv(dateset, 
          "~/Desktop/hindoor.csv", 
          row.names = TRUE)

# indoor outdoor ratio
# plot infiltration ratio
hindoor = read.csv(file = "hindoor_r.csv")
View(hindoor)
dfindoor <- data.frame(hindoor)
View(dfindoor)

# basic scatter plot for slope of indoor outdoor ratio
# remove intercept
library(ggpmisc)
myformula <- y ~ x
jpeg("slopepm2.5.jpg", width = 8, height = 5, units = 'in', res = 600)
po <- ggplot(dfindoor, aes(x = ROutPM25, y = RinPM25)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = TRUE) + 
  stat_poly_eq(use_label(c("eq", "adj.R2","p", "n")), coef.keep.zeros = TRUE) + 
  scale_x_continuous(name = "Outdoor PM2.5 (ug/m3)") + 
  scale_y_continuous(name = "Indoor PM2.5 (ug/m3)") + 
  labs(title = "Scatter plot of Indoor PM2.5 vs Outdoor PM2.5")
dev.off() 
#
jpeg("aslopepm2.5.jpg", width = 8, height = 5, units = 'in', res = 600)
pa <- ggplot(dfindoor, aes(x = Ambient.PM2.5, y = RinPM25)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = TRUE) + 
  stat_poly_eq(use_label(c("eq", "adj.R2","p", "n")), coef.keep.zeros = TRUE) + 
  scale_x_continuous(name = "Ambient PM2.5 (ug/m3)") + 
  scale_y_continuous(name = "Indoor PM2.5 (ug/m3)") + 
  labs(title = "Scatter plot of Indoor PM2.5 vs Ambient PM2.5")
dev.off() 
# 
# different display of figures
jpeg("slopeoa.jpg", width = 10, height = 5, units = 'in', res = 600)
ggarrange(po, pa,
          labels = c("a", "b"),
          ncol = 2,
          nrow = 1)
dev.off() 

# hourly variation
library(tidyquant)
dfindoor <- dfindoor %>% 
  mutate(Date = as.Date(Data))
dfindoor
# indoor to outdoor ratio
ratiopm25 <- dfindoor %>% group_by(Date, h) %>%
  summarise(mean = mean(I.O.PM25))
ratiopm25
# indoor to ambient ratio
aratiopm25 <- dfindoor %>% group_by(Date, h) %>%
  summarise(mean = mean(I.A.PM25))
aratiopm25

## hourly variation PM2.5
io <- ratioPM25 <- ggplot(ratiopm25, aes(x = h, y = mean)) + 
  geom_boxplot(aes(group = h), color = "darksalmon", fill = "salmon", alpha = 0.4) + 
  scale_x_continuous(name = "Hour", limits = c(0, 23), breaks = seq(0, 23, 1)) +
  scale_y_continuous(name = "Indoor to outdoor ratio", limits = c(0.2,1), breaks = seq(0, 1, 0.2)) + 
  labs("PM2.5") + 
  theme(panel.grid.minor = element_blank())
ia <- aratioPM25 <- ggplot(aratiopm25, aes(x = h, y = mean)) + 
  geom_boxplot(aes(group = h), color = "darkseagreen3", fill = "green3", alpha = 0.4) + 
  scale_x_continuous(name = "Hour", limits = c(0, 23), breaks = seq(0, 23, 1)) +
  scale_y_continuous(name = "Indoor to ambient ratio", limits = c(0.2,1), breaks = seq(0, 1, 0.2)) + 
  labs("PM2.5") + 
  theme(panel.grid.minor = element_blank())
# different display of figures
jpeg("ioa ratio.jpg", width = 10, height = 5, units = 'in', res = 600)
ggarrange(io, ia,
          labels = c("a", "b"),
          ncol = 2,
          nrow = 1)
dev.off() 
###
# dirunal pattern of PM2.5 indoor and outdoor
# outdoor PM2.5
opm25 <- dfindoor %>% group_by(Date, h) %>%
  summarise(mean = mean(ROutPM25))
opm25
# ambient PM2.5
apm25 <- dfindoor %>% group_by(Date, h) %>%
  summarise(mean = mean(Ambient.PM2.5))
apm25
# indoor PM2.5
ipm25 <- dfindoor %>% group_by(Date, h) %>%
  summarise(mean = mean(RinPM25))
ipm25
# 
## hourly variation PM2.5
o <- oPM25 <- ggplot(opm25, aes(x = h, y = mean)) + 
  geom_boxplot(aes(group = h), color = "darkslategray4",fill = "darkslategray3", alpha = 0.4) + 
  scale_x_continuous(name = "Hour", limits = c(0, 23), breaks = seq(0, 23, 1)) +
  scale_y_continuous(name = "Outdoor PM2.5 (ug/m3)", limits = c(0,20), breaks = seq(0, 20, 10)) + 
  labs("PM2.5") + 
  theme(panel.grid.minor = element_blank())
o
#
a <- aPM25 <- ggplot(apm25, aes(x = h, y = mean)) + 
  geom_boxplot(aes(group = h), color = "gold3", fill = "gold1", alpha = 0.4) + 
  scale_x_continuous(name = "Hour", limits = c(0, 23), breaks = seq(0, 23, 1)) +
  scale_y_continuous(name = "Ambient PM2.5 (ug/m3)", limits = c(0,20), breaks = seq(0, 20, 10)) + 
  labs("PM2.5") + 
  theme(panel.grid.minor = element_blank())
a
#
i <- aPM25 <- ggplot(ipm25, aes(x = h, y = mean)) + 
  geom_boxplot(aes(group = h), color = "steelblue2", fill = "steelblue", alpha = 0.4) + 
  scale_x_continuous(name = "Hour", limits = c(0, 23), breaks = seq(0, 23, 1)) +
  scale_y_continuous(name = "Indoor PM2.5 (ug/m3)", limits = c(0,15), breaks = seq(0, 15, 5)) + 
  labs("PM2.5") + 
  theme(panel.grid.minor = element_blank())
i
# 

# different display of figures
jpeg("ioa con.jpg", width = 15, height = 5, units = 'in', res = 600)
ggarrange(o, a, i, 
          labels = c("a", "b", "c"),
          ncol = 3,
          nrow = 1)
dev.off() 

# plot diurnal variation using geomribbon


jpeg("try.jpg", width = 15, height = 5, units = 'in', res = 600)
ggplot(ipm25) + 
  geom_ribbon(aes(x = h, ymin = min(ipm25$iPM2.5, na.rm = TRUE), 
                  ymax = max(dfindoor$iPM2.5, na.rm = TRUE))) + 
  geom_line(aes(x = h, y = mean))
dev.off() 

#####
# outdoor 
outdoor = read.csv(file = "16383_1695308751721-outdoor.csv")
View(outdoor)
outdoor <- na.omit(outdoor)
dfoutdoor <- data.frame(outdoor)
View(dfoutdoor)
colnames(dfoutdoor) <- c('time', 'RH', 'T', 'P', 'Battery','eCO2', 'TVOC', 'light','PM1', 'noise', 'PM10','PM25')
colnames(dfoutdoor)

# dateonly
data <-as.POSIXct(format(as.POSIXct(dfoutdoor$time), tz = "UTC"))
dateonly <-as.Date(data)
head(dateonly)

# add date
library(tidyquant)
dfoutdoor <- dfoutdoor %>% 
  mutate(Date = as.Date(time))
dfoutdoor
datetime_h <- hour(ymd_hms(dfoutdoor$time, tz = "Europe/Dublin"))
dfoutdoor <- dfoutdoor %>% 
  mutate(dfoutdoor, h = datetime_h)
dfoutdoor
# pm2.5
hPM2.5 <- dfoutdoor %>% group_by(Date, h) %>%
  summarise(Mean = mean(PM25))
hPM2.5
# pm10
hPM10 <- dfoutdoor %>% group_by(Date, h) %>%
  summarise(Mean = mean(PM10))
hPM10
# pm1
hPM1 <- dfoutdoor %>% group_by(Date, h) %>%
  summarise(Mean = mean(PM1))
hPM1
# tep
T <- dfoutdoor %>% group_by(Date, h) %>%
  summarise(Mean = mean(T))
T
# rh
RH <- dfoutdoor %>% group_by(Date, h) %>%
  summarise(Mean = mean(RH))
RH
# pressure
P <- dfoutdoor %>% group_by(Date, h) %>%
  summarise(Mean = mean(P))
P
# eCO2
eCO2 <- dfoutdoor %>% group_by(Date, h) %>%
  summarise(Mean = mean(eCO2))
eCO2
# TVOC
TVOC <- dfoutdoor %>% group_by(Date, h) %>%
  summarise(Mean = mean(TVOC))
TVOC
# TVOC
light <- dfoutdoor %>% group_by(Date, h) %>%
  summarise(Mean = mean(light))
light
# noise
noise <- dfoutdoor %>% group_by(Date, h) %>%
  summarise(Mean = mean(noise))
noise
# export the dataframe with new columns of seperated date time
library(data.table)
library(tibble)
library(xts)
library(lubridate)
library(tidyverse)
dateset <- list(hPM2.5,hPM10, hPM1, T, RH, P, eCO2, TVOC, light, noise)
write.csv(dateset, 
          "~/Desktop/houtdoor.csv", 
          row.names = TRUE)
###
# ambient
ambient = read.csv(file = "Air quality levels at Clonskeagh Dublin 14.csv")
View(ambient)
dfambient <- data.frame(ambient)
colnames(dfambient) <- c('time', 'O3', 'PM10','PM25')
colnames(dfambient)

# plot
library(ggplot2)
library(tidyverse)
library(lme4)
library(ggpubr)
library(ggpmisc)

setwd("~/Desktop/AIVC2023")
p = read.csv(file = "houtdoor_r1.csv")
View(p)
p <- data.frame(p)
# basic scatter plot for PM2.5
jpeg("scatterplot.jpg", width = 8, height = 5, units = 'in', res = 600)
ggplot(p, aes(x = Ambient.PM2.5, y = oMeanpm25)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = TRUE) + 
  stat_poly_eq(use_label(c("eq", "adj.R2","p", "n"))) + 
  scale_x_continuous(name = "Ambient PM2.5 (ug/m3)") + 
  scale_y_continuous(name = "Outdoor PM2.5 (ug/m3)") + 
  labs(title = "Scatter plot of Ambient PM2.5 vs Outdoor PM2.5")
dev.off() 

# # basic scatter plot for PM10
jpeg("scatterplotpm10.jpg", width = 8, height = 5, units = 'in', res = 600)
ggplot(p, aes(x = PM10, y = oMeanpm10)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = TRUE) + 
  stat_poly_eq(use_label(c("eq", "adj.R2","p", "n"))) + 
  scale_x_continuous(name = "Ambient PM10 (ug/m3)") + 
  scale_y_continuous(name = "Outdoor PM10 (ug/m3)") + 
  labs(title = "Scatter plot of Ambient PM10 vs Outdoor PM10")
dev.off()


# plot two variables
# dateonly
Date <-as.Date(as.character(p$Date), format = "%Y-%m-%d")
head(Date)
View(p)
hPM2.5 <- p %>% group_by(Date, h) %>%
  summarise(mean = mean(Outdoor.PM2.5))
hPM2.5
jpeg("timeser.jpg", width = 8, height = 5, units = 'in', res = 600)
ggplot(hPM2.5, aes(x = Date, y = mean)) + 
  geom_line()
dev.off() 

###
#indoor
# calculate corr
hindoor = read.csv(file = "hindoor_r.csv")
View(hindoor)
dfindoor <- data.frame(hindoor)

indoor <- dfindoor[, c("iPM2.5", "iPM10", "iPM1",
                       "iT", "iRH",
                       "ieCO2", "iTVOCs")]
indoor1 <- indoor %>% drop_na()

# load library
library(ggcorrplot)
library(ggplot2)
library(corrplot)
library(Hmisc)
M = cor(indoor1)
corrplot(M, method = "circle")
ggcorrplot(M)
jpeg("indoor cor.jpg", width = 8, height = 5, units = 'in', res = 600)
ggcorrplot(M)
dev.off() 

# outdoor
outdoor <- dfindoor[, c("oPM2.5", "oPM10", "oPM1",
                       "oT", "oRH",
                       "oeCO2", "oTVOCs")]
outdoor1 <- outdoor %>% drop_na()
Mo = cor(outdoor1)
jpeg("outdoor cor.jpg", width = 8, height = 5, units = 'in', res = 600)
ggcorrplot(Mo)
dev.off() 

# indoor and outdoor
inout <- dfindoor[, c("oPM2.5", "oPM10", "oPM1", 
                      "oT", "oRH", "oeCO2", "oTVOCs", 
                      "iPM2.5", "iPM10", "iPM1", 
                      "iT", "iRH", "ieCO2", "iTVOCs")]
inout1 <- inout %>% drop_na()
Mio = cor(inout1)
jpeg("inout corr.jpg", width = 8, height = 5, units = 'in', res = 400)
ggcorrplot(Mio)
dev.off()
