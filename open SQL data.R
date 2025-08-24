# packages
install.packages("lubridate")
install.packages("xts")
install.packages("tidyverse")
install.packages("ggplot2")

## read SQLite .db file into R
library(RSQLite)
library(DBI)
library(ProjectTemplate)
library(dplyr)
library(ggplot2)
library(ggpubr)
setwd("~/Desktop/TwinAIR/Google AirView_Dublin/AirViewdata")
# filename <- "GoogleAQdata.db"

## sqliter driver
sqlite.driver <- dbDriver("SQLite")

# creat a connect
db <-dbConnect(sqlite.driver, dbname = filename)

# some operations
dbListTables(db)

# to read one of the tables:
saved <- dbReadTable(db, "RawData")
saved

# summary the data
summary(saved)
# display the rows and columns of the dataset
dim(saved)

# rows = 5030143 columns = 29
# first few rows of the dataset
head(saved)
str(saved)
nrow(saved[!is.na(saved$pm25_halfLOD),])
nrow(saved[!is.na(saved$co2_ppm_halfLOD),])
nrow(saved[!is.na(saved$co_ppm_halfLOD),])
nrow(saved[!is.na(saved$o3_ppb_halfLOD),])
nrow(saved[!is.na(saved$no2_ppb_halfLOD),])
nrow(saved[!is.na(saved$no_ppb_halfLOD),])
summary(saved$no_ppb_halfLOD)

+++++++++++++++++++++++
## density plot
## q-q plot
# PM2.5
library(ggpubr)
ggdensity(saved$pm25_halfLOD,
          main = "Density plot of PM2.5 concentration",
          xlab = "PM2.5 concentration (ug/m3)")
ggqqplot(saved$pm25_halfLOD)
quantile(saved$pm25_halfLOD, c(0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99))

# NO2
library(ggpubr)
ggdensity(saved$no2_ppb_halfLOD,
          main = "Density plot of NO2 concentration",
          xlab = "NO2 concentration (ppb)")
ggqqplot(saved$no2_ppb_halfLOD)
quantile(saved$no2_ppb_halfLOD, c(0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99))

#NO
library(ggpubr)
ggdensity(saved$no_ppb_halfLOD,
          main = "Density plot of NO concentration",
          xlab = "NO concentration (ppb)")
ggqqplot(saved$no_ppb_halfLOD)
quantile(saved$no_ppb_halfLOD, c(0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99))

# O3
library(ggpubr)
ggdensity(saved$o3_ppb_halfLOD,
          main = "Density plot of O3 concentration",
          xlab = "O3 concentration (ppb)")
ggqqplot(saved$o3_ppb_halfLOD)
saved$o3_ppb_halfLOD <- 
quantile(saved$o3_ppb_halfLOD,c(0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99), na.rm = TRUE)

# CO
ggdensity(saved$co_ppm_halfLOD,
          main = "Density plot of CO concentration",
          xlab = "CO concentration (ppm)")
ggqqplot(saved$co_ppm_halfLOD)
quantile(saved$co_ppm_halfLOD, c(0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99))

#CO2
ggdensity(saved$co2_ppm_halfLOD,
          main = "Density plot of CO2 concentration",
          xlab = "CO2 concentration (ppm)")
ggqqplot(saved$co2_ppm_halfLOD)
quantile(saved$co2_ppm_halfLOD, c(0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99))
++++++++++++++++++++++++++++++

++++++++++++++++++++++++++++++
# creat a dataframe
df <- data.frame(saved)
print(df)

# computing moving average using rollmean() Function of zoo package
# install zoo package #load package
library(zoo)
library(ggplot2)
moving_avg <-rollmean(df$pm25_halfLOD, k=5)
moving_avg
moving_median <-rollmedian(df$pm25_halfLOD, k=5)
moving_median
moving_sum <-rollsum(df$pm25_halfLOD, k=5)
moving_sum

# datetime
alldata1 <-as.POSIXct(format(as.POSIXct(saved$gps_timestamp), tz = "UTC"), tz = "GMT")
dateonly <-as.Date(alldata1)
head(dateonly)


#convert data to time series
library(xts)
dateonly_ts <- xts(saved$pm25_halfLOD, dateonly)
dateonly_ts

# open a jpeg file
jpeg("rplot.jpg", width = 8, height = 5, units = 'in', res = 500)

# creat a plot
plot(dateonly, df$pm25_halfLOD, type = "l",   # Plotting series & moving metrics
     main = "PM2.5 pollution over time",
     ylim = c(min(df$pm25_halfLOD), max(df$pm25_halfLOD)),
     xlab = "Date", ylab = "PM2.5 concentration (ug/m3)")
axis(1, dateonly, format(dateonly, "%Y-%m"))
lines(dateonly, c(NA, NA, moving_avg, NA, NA), type = "l", col = 2)
lines(dateonly, c(NA, NA, moving_median, NA, NA), type = "l", col = 3)
legend("topleft",
       c("Time Series", "Moving Average", "Moving Median"),
       lty = 1, col = 1:3)
#close the pdf
dev.off()
+++++++++++++++++++++++++++++++++++++++++++++++
  
# cut second to min
library(lubridate)
library(tidyverse)
library(data.table)
library(tibble)
library(xts)
help(lubridate)
# alldata <- data.table(as.POSIXct(format(as.POSIXct(saved$gps_timestamp), tz = "UTC"), tz = "GMT")) 
# str(alldata)
alldata1 <-as.POSIXct(format(as.POSIXct(saved$gps_timestamp), tz = "UTC"), tz = "GMT")
str(alldata1)

# per 3min
avg_3alldata <-mutate(interval = lubridate::minute(alldata1) %/% 3)
avg_nox <- period.apply(saved$nox_derived, endpoints(alldata, "minutes", 3), mean)
avg_no <- period.apply(saved$no_ppb_halfLOD, endpoints(alldata, "minutes", 3), mean)
avg_no2 <- period.apply(saved$no2_ppb_halfLOD, endpoints(alldata, "minutes", 3), mean)
avg_o3 <- period.apply(saved$o3_ppb_halfLOD, endpoints(alldata, "minutes", 3), mean)
avg_co <- period.apply(saved$co_ppm_halfLOD, endpoints(alldata, "minutes", 3), mean)
avg_co2 <- period.apply(saved$co2_ppm_halfLOD, endpoints(alldata, "minutes", 3), mean)
avg_pm25 <- period.apply(saved$pm25_ppm_halfLOD, endpoints(alldata, "minutes", 3), mean)


mutate()
data.table(avg)

# data frame
p1 <- saved$co2_ppm_halfLOD
saved1 <- data.frame(alldata, p1)

# plot figures
library(ggplot2)
library(tidyverse)
ggplot(saved1, aes(x = alldata, y = p1)) + geom_line()

# fit distributions
install.packages("fitdistrplus")
library(fitdistrplus)
descdist(saved$pm25_halfLOD, discrete = FALSE)
normal_dis <-fitdist(saved$pm25_halfLOD,"norm")
plot(normal_dis)


#20230419
# data.table for big data
install.packages("data.table")
install.packages(c("nycflights13","gapminder","Lahman"))
library(data.table)

## update r and Rstudio
install.packages("devtools")
install.packages("updater")
help("updater")
library(updater)
data.table::update_dev_pkg()

## dplyr basics
## pick observations by their values (filter())
## reorder the rows (arrange())
## pick variables by names (select())
## creat new variables with functions of existing variables (mutate())
## collapse many values down to a single summary (summarise())
## add new variables with mutate()
## if you only want to keep the new variables transmute()

###############################3
# disconnect
dbDisconnect(db)

# 20230331
# connection to a SQL database
# libraries
library(RSQLite)
library(DBI)
# connect to the database
setwd("~/Desktop/TwinAIR/Google AirView_Dublin/data")
connection <-dbConnect(
  drv = SQLite(),
  dbname = "sql_code.sql", synchronous = NULL 
)
# fetch results
tbl(connection, "GoogleAQdata_data") %>%
  collect() -> data
# disconnect
dbDisconnect(connection)
# preview
data
###########################33
