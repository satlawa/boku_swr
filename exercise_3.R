### Task 1 – Statistical Tests
require(nortest)
require(exactRankTests)
require(car)

# our sample of size n = 18
x <- c(870, 930, 932, 935, 938, 1045, 1050, 1052, 1055,
       970, 980, 1001, 1009, 1027, 1030, 1032, 1040, 1046)

# check if the data has normal distribution
# normality test for old machine
lillie.test(x = x)
# the data is not passing the test for normal distribution, hence we cannot use the one-sample t-test, but instead use a non parametric test.

## 1. check, if the mean length (in mm) of these components is mu 0 = 950, as it should be according to technical specifications
# no 1-sample t-test, two-sided since it is not passing the test for normal distribution
#t.test(x = x, alternative = "two.sided", mu = 950)

# Wilcoxon Signed Rank Test
wilcox.exact(x = x, alternative = "two.sided", mu = 950)
# the p value of 0.00321 is lower than 0.05, hence we reject H0 that the mean length of the produced components is 950mm.

# second test sign test
# reference value
M0 <- 950
# differences to reference
(diff_lenghts <- x - M0)
z <- numeric(length = length(diff_lenghts))
# difference of 0 does not occur in this example
z[diff_lenghts > 0] <- 1
z[diff_lenghts < 0] <- 0
# test statistic
(test_statistic <- sum(z))
# two sided binomial test with
# x = number of successes = number of ones in z
binom.test(x = sum(z), n = length(diff_lenghts), p = 0.5, alternative = "two.sided")
# As the p-value is larger than α = 0.05, there is no evidence in the data to reject H 0 – we still may assume that M = 950mm
# however we have the assumption that the data is symmetric that is not the case.


## 2. variance in the item length has changed with the new machine

# our sample of size n = 10 from new machine
y <- c(1025, 1006, 1005, 1000, 990, 1013, 912, 1011, 909, 947)

# normality test for new machine
lillie.test(y = y)
# the data is not passing the test for normal distribution, similarly to the data of the first machine

# no F-Test
#var.test(x = x, y = y, alternative = "two.sided")
# since we cannot use the F-test because the data is not normally distributed we will use the Levene Test

z <- c(x, y)
group <- as.factor(c(rep(1, length(x)), rep(2, length(y))))

# Levene Test
leveneTest(z, group, center = "mean")
# p-value of the Levene Test is 0.262, hence we cannot reject H0 stating that the varinaces of the two samples are not equal

## 3. mean length has changed with the new machine

# data frame with scores, group and ranks
(data_frame <- data.frame(len = c(x, y),
                         group = c(rep("X", length(x)), rep("Y", length(y))),
                         rank = rank(c(x, y))))

# 2 sample Wilcoxon Rank Sum Test, 2-sided and equal variances
wilcox.test(len ~ group, data = data_frame, alternative = "two.sided", exact = TRUE)
#We obtain a p-value of 0.226 hence we do not reject H 0 stating that the equal mean length of both machines does not differ.

## 4. 

# boxplot of ozone levels in May and August
boxplot(len ~ group, data = data_frame,
        col = heat.colors(2),
        main = "Ozone Levels in May and August",
        names = c("old", "new"), ylab = "Length [mm]")



### Task 2 - Functions

## 1. Create a function with the name quad_equ()

quad_equ <- function(coef){
  x1 <- -(coef[1]/2) - sqrt((coef[1]/2)^2-coef[2])
  x2 <- -(coef[1]/2) + sqrt((coef[1]/2)^2-coef[2])
    
  return(c(x1,x2))
}
coef <- c(3,-4)
quad_equ(coef)


## 2. Extend function - check if the passed argument coef gives real solutions

quad_equ <- function(coef){
  if((coef[1]/2)^2-coef[2] >= 0){
    x1 <- -(coef[1]/2) - sqrt((coef[1]/2)^2-coef[2])
    x2 <- -(coef[1]/2) + sqrt((coef[1]/2)^2-coef[2])
    
    return(c(x1,x2))
  } else{
    stop("The supplied vector x has no real solution.")
  }
}
coef <- c(3,-4)
quad_equ(coef)
coef <- c(-2, 2)
quad_equ(coef)


## 3. Extend function - check if the passed argument is valid 

quad_equ <- function(coef){
  if(length(coef)==2){
    if((coef[1]/2)^2-coef[2] >= 0){
      x1 <- -(coef[1]/2) - sqrt((coef[1]/2)^2-coef[2])
      x2 <- -(coef[1]/2) + sqrt((coef[1]/2)^2-coef[2])
      
      return(c(x1,x2))
    } else{
      stop("The supplied vector x has no real solution.")
    }
  } else{
    stop("The supplied vector x has the wrong length. 
         The input vector has to have length 2. 
         Please enter a valid vector.")
  }
}
# test if the function is working correctly with a correct input
coef <- c(3,-4)
quad_equ(coef)
# test if the function invokes an error with an incorrect input
coef <- c(3,-4, 9)
quad_equ(coef)


### Task 3 - Graphics

# creating data

# Load ggplot
library(ggplot2)
library(data.table)
# Read in dataset
data(iris)

# create data
dt <- data.table(id = c(1:29), age = c(25, 21, 5, 15, 47, 33, 39, 56, 3, 45, 31, 28, 44, 15, 13, 22, 40, 39, 59, 13, 37, 14, 60, 44, 25, 21, 35, 33, 29))

breaks <- c(0,11,21,31,41,51,61)
tags <- c("0-10","11-20", "21-30", "31-40", "41-50", "51-60")
group_tags <- cut(dt$age, breaks=breaks, include.lowest=TRUE, right=FALSE, labels=tags)
# add to dt
dt[, ("age_category") := group_tags]

# plot
p <- ggplot(data = dt, mapping = aes(x=age_category)) + 
  geom_bar(stat="count", fill="orange", color="black", width = 1) +
  labs(title = "Number of People in Each Age Category", x = "Age Category", y = "People") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "grey85"),
        panel.grid.major.x = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_text(colour = "grey30"),
        plot.title = element_text(colour = "grey30")) +
  scale_y_continuous(breaks = seq(0, 9, 1), expand=c(0,0), limits=c(0,9))
p

