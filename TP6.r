#
#
# $$$$$$$    Finding CI from runif function     $$$$$$$$
#
#



LO = -2 # Declaration of lower bownd
UP = 3 # Declaration of upper bownd
n = 1000 # Declaration of sample size

x <- runif(n, LO, UP) # Running runif function to create population
hist(x, freq = FALSE, xlab = 'x', density = 20) # Histogram of population
x # all generated values

#x <- qnorm(runif(n, pnorm(LO), pnorm(UP))) # Normalizing the population to generate a destribution curve with quantile function qnorm
#hist(x, freq = FALSE, xlab = 'x', density = 20) # Histogram after normaization

# Plotting x
plot(density(x, adjust = 2))



stddev = sd(x) # Calculatiing standerd deviation
stddev # standerd deviation
center = mean(x) # Calculating mean
center # mean



# Upper (Right) Risk

error <- qnorm(0.95)*stddev/sqrt(n) # Calculating Error Rate according to 95% of population
error # Error rate

upper_bound <- UP - (center + error) # calculating upper bound (risk a droit)
upper_bound # upper bound


right_risk <- which(x > upper_bound) # Finding values that are in the lower bound risk
x[right_risk]  # values at upper risk






# Lower (Left) Risk

error <- qnorm(0.95)*stddev/sqrt(n) # Calculating Error Rate according to 95% of population
error # Error rate

lower_bound <- LO +(center - error) # Calculating lower bound (risk a gouche)
lower_bound # lower bound


left_risk <- which(x  < lower_bound) # Finding values that are in the upper bound risk
x[left_risk] # values at lower risk 







# Balanced Risk Left and Right

error <- qnorm(0.975)*stddev/sqrt(n) # Calculating Error Rate according to 95% of population
error # Error rate

lower_bound <- LO +(center - error) # Calculating lower bound (risk a gouche)
lower_bound # lower bound


upper_bound <- UP - (center + error) # calculating upper bound (risk a droit)
upper_bound # upper bound

right_risk <- which(x > upper_bound) # Finding values that are in the lower bound risk
x[right_risk]  # values at upper risk

left_risk <- which(x  < lower_bound) # Finding values that are in the upper bound risk
x[left_risk] # values at lower risk 








#
#
# $$$$$$$    Finding CI from rnorm function     $$$$$$$$
#
#






LO = -2 # Declaration of lower bownd
UP = 3 # Declaration of upper bownd
SD = 0.9 # Assumed Standard deviation 
ME = 0.05 # Assumed Mean
n = 1000 # Declaration of sample size



#' Creates a random normal distribution within the specified bounds
#' 
#' WARNING: This function does not preserve the standard deviation
#' @param n The number of values to be generated
#' @param mean The mean of the distribution
#' @param sd The standard deviation of the distribution
#' @param lower The lower limit of the distribution
#' @param upper The upper limit of the distribution
# rtnorm <- function(n, mean = ME, sd = SD, lower = LO, upper = UP){
#     mean = ifelse(test = (is.na(mean)|| (mean < lower) || (mean > upper)),
#                   yes = mean(c(lower, upper)),
#                   no = mean)
#     data <- rnorm(n, mean = mean, sd = sd) # data
# 
#     if (!is.na(lower) && !is.na(upper)){ # adjust data to specified range
#         drange <- range(data)            # data range
#         irange <- range(lower, upper)    # input range
#         data <- (data - drange[1]) / (drange[2] - drange[1]) # normalize data (make it -2 to 3)
#         data <- (data * (irange[2] - irange[1])) + irange[1] # adjust to specified range
#     }
#     return(data)
# }


# x <- rtnorm(n = n) # Using a function to create random population with rnorm with specific range between -2 and 3


x <- rnorm(n, mean = ME, sd = SD) # data generated from rnorm as normal distibution
x # Created data

x <- x[x < 3]   # Removing right side outlier values
x <- x[x > -2]  # Removing left side outlier values
x # values after removing outliers


hist(x, freq = FALSE, xlab = 'x', density = 20) # Histogram of the values


# Plotting x
plot(density(x , adjust = 2))



# Upper (Right) Risk

error <- qnorm(0.95)*SD/sqrt(n) # Calculating Error Rate according to 95% of population
error # Error rate

upper_bound <- UP - (ME + error) # calculating upper bound (risk a droit)
upper_bound # upper bound


right_risk <- which(x > upper_bound) # Finding values that are in the lower bound risk
x[right_risk]  # values at upper risk







# Lower (Left) Risk

error <- qnorm(0.95)*SD/sqrt(n) # Calculating Error Rate according to 95% of population
error # Error rate

lower_bound <- LO +(ME - error) # Calculating lower bound (risk a gouche)
lower_bound # lower bound


left_risk <- which(x  < lower_bound) # Finding values that are in the upper bound risk
x[left_risk] # values at lower risk 






# Balanced Risk Left and Right

error <- qnorm(0.975)*SD/sqrt(n) # Calculating Error Rate according to 95% of population
error # Error rate

lower_bound <- LO +(ME - error) # Calculating lower bound (risk a gouche)
lower_bound # lower bound


upper_bound <- UP - (ME + error) # calculating upper bound (risk a droit)
upper_bound # upper bound

right_risk <- which(x > upper_bound) # Finding values that are in the lower bound risk
x[right_risk]  # values at upper risk

left_risk <- which(x  < lower_bound) # Finding values that are in the upper bound risk
x[left_risk] # values at lower risk 










#
#
# $$$$$$$    Finding CI from rbinom function     $$$$$$$$
#
#




LO = -2 # Declaration of lower bownd
UP = 3 # Declaration of upper bownd
SD = 0.9 # Assumed Standard deviation 
ME = 0.05 # Assumed Mean
ER = 0.05 # ERROR Rate
n = 1000 # Declaration of sample size


x = rbinom(n, 1, SD) # sample

y = sum(x) # Success in the sample

binom.test(y, n, conf.level = 0.95) # using buit in function to find confidence interval 

