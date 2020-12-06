# install library
install.packages("data.table")
# load library
library("data.table")

### TASK 3

## 1. load data

# load data Rdata
load("~/Documents/boku/statistics_with_R/ex_02/CO2.Rdata")
str(dat)

# load data txt
data_txt <- read.table(file = "~/Documents/boku/statistics_with_R/ex_02/CO2.txt", header = TRUE, dec = ".")
str(data_txt)

# load data csv
data_csv <- read.csv(file = "~/Documents/boku/statistics_with_R/ex_02/CO2.csv", header = TRUE, row.names = 1)
str(data_txt)

# load data xlsx

## 2. set seed with id
set.seed(as.numeric(format(Sys.time(), "%H%M%S")))
id <- 640348
set.seed(id)

## 3. examine data

str(dat)
summary(dat)
# 6 variables (features in columns) 
# unit     : Factor (categorical) w/ 2 levels "MIO_T","THS_T"
# airpol   : Factor (categorical) w/ 11 levels "CH4","CH4_CO2E"
# airemsect: Factor (categorical) w/ 172 levels "CRF1","CRF1-6X4_MEMO"
# geo      : Factor (categorical) w/ 35 levels "AT","BE","BG"
# time     : numeric (int) -> year
# values   : numeric (float/double) -> value

# NA in values 5284 records

# convert to data.table
datCO2 <- setDT(dat)


## 4. create vector out of column and select randomly 2 countries

geo_col <- datCO2[, unique(geo)]
geo_c <- sample(geo_col, 2)
#levels(eye_colors_fac)

## 5. filter data
datFilter <- datCO2[unit == "THS_T" & 
                      airpol == "GHG" & 
                      airemsect %in% c("CRF3", "CRF31", "CRF1A3") & 
                      geo %in% geo_c]

## 6. remove columns unit and airpol
datFilter <- datFilter[, -c("unit", "airpol")]

## 7. observations per country
datFilter[, .N, by = geo]

## 8. rename


## 9. average greenhouse emissions per sector
datFilter[,.(mean(values)), by = airemsect, ]

## 10. average greenhouse emissions per sector and country
datFilter[,.(mean(values)), by = .(airemsect, geo)]

## 11. sum of greenhouse emissions in the "Livestock" sector per country
datFilter[airemsect == "CRF31" & time %between% c(2000, 2017), .(sum(values)), by = .(geo)]

## 12. plot
library(ggplot2)
ggplot(data=datFilter[airemsect == "CRF31"], aes(x=time, y=values, color=geo)) + geom_line()