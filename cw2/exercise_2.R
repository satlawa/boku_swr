install.packages("matrixStats")
install.packages("data.table")

### TASK 1 – Robustness of mean/median
library("matrixStats")

## 1. compare mean and median - without outliers
set.seed(123)
x <- rnorm(n = 50 * 1000, mean = 0, sd = 1)
# matrix with 1000 rows (N) and 50 columns (n)
X <- matrix(x, ncol = 50)

# calculate the mean for every sample
xAvg <- rowMeans(X)

# calculate the median for every sample
xMed <- rowMedians(X)

boxplot(xAvg, xMed)
#ggplot(mpg, aes(class, hwy)) + geom_boxplot()

# The mean estimation method provides a much better result from a normal distribution without outlies are in the dataset

## 2. compare mean and median - with outliers
set.seed(123)

# sample from normal distribution (95%) and sample from Exponential Distribution (5%)
x95 <- rnorm(n = 45 * 1000, mean = 0, sd = 1)
x05 <- rexp(n = 5 * 1000, rate = 0.2)

# rearrange vector to a matrix
x95 <- matrix(x95, ncol = 45)
x05 <- matrix(x05, ncol = 5)

# concatenate the two matrices
X <- cbind(x95,x05)

# calculate the mean for every sample
xAvg <- rowMeans(X)

# calculate the median for every sample
xMed <- rowMedians(X)

boxplot(xAvg, xMed)

# The median estimation method provides a much better result in the case outlies are in the dataset



### TASK 2 – Estimation

## 1. Standard Cauchy Distribution with diffrent sample sizes
cuy100 = rcauchy(100, location = 0, scale = 1)
cuy5000 = rcauchy(5000, location = 0, scale = 1)
cuy100000 = rcauchy(100000, location = 0, scale = 1)

(mean(cuy100))
(mean(cuy5000))
(mean(cuy100000))

(var(cuy100))
(var(cuy5000))
(var(cuy100000))

## 2.

## 3.
par(mfrow = c(1,2))
boxplot(rnorm(n = 100, mean = 0, sd = 1)) # ... function for normal boxplot
boxplot(rcauchy(n = 100, location = 0, scale = 1)) # ... function for cauchy boxplot
par(mfrow = c(1,1))

# The Standard Cauchy Distribution creates more outliers the bell curve is much wider compared to the normal distribution



### TASK 3
library("data.table")

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
datFilter[airemsect == "CRF1A3", airemsect := "Transport"]
datFilter[airemsect == "CRF3", airemsect := "Agriculture"]
datFilter[airemsect == "CRF31", airemsect := "Livestock"]


## 9. average greenhouse emissions per sector
datFilter[,.(mean(values)), by = airemsect, ]

## 10. average greenhouse emissions per sector and country
datFilter[,.(mean(values)), by = .(airemsect, geo)]

## 11. sum of greenhouse emissions in the "Livestock" sector per country
datFilter[airemsect == "Livestock" & time %between% c(2000, 2017), .(sum(values)), by = .(geo)]

## 12. plot
library(ggplot2)
ggplot(data=datFilter[airemsect == "Transport"], aes(x=time, y=values, color=geo)) + geom_line()