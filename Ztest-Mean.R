
# Search in internet: the avarage adult male height 
# in the US is 69.5 inch and the stdev is 4 inch. 
# Population height is normally distributed.

# This semester I gathered data from my MBA Stat Class
data <- read.csv("http://tiny.cc/fa18classData")
head(data)

# I want to test whether the adult male height is 69.5 or not
# H0: mu  = 69.5
# Ha: mu neq 69.5 

# Data is a mix of male and female students so I need to filter my data:
data.m = data[data$gender=="Male",]

# null Hypothesis
mu0 = 69.5
sigma = 4 #sigma is known.

# Sampling information:
n = length(data.m$height)
n
xbar = mean(data.m$height) 
xbar

# find test Statistic
Z0 = (xbar-mu0)/(sigma/sqrt(n))
Z0

# Check the significance. 
alpha = 0.05

# Two tail test:
(Z0 > qnorm(1-alpha/2))||(Zstat < -qnorm(1-alpha/2))
# Fial to reject null hypthesis at alpha level of significance.

# What happened graphically:
z = seq(-4, 4, 0.01) # making sequnce of numbers between -4 and 4
plot(z, dnorm(z), main = "H0 distribution", type = "l", yaxs="i")
abline(v=c(-qnorm(1-alpha/2), qnorm(1-alpha/2)), col = "red") # Critical value
abline(v = Z0 , col = "blue") # Z0

# finding p-value
# pvalue = 2*P(Z > Z0)
2*(1-pnorm(Z0))

# How to understand this by simulation:
# Simulate the null distribution by normall distribution:
set.seed(123)
nsim = 10000
ntot = n*nsim
rv = rnorm(ntot, mu0, sigma) 
rvm = matrix(rv, nrow = nsim)
xbars.sim = rowMeans(rvm)
hist(xbars.sim, freq=F, main ="simulated H0 Histogram for Xbars")
abline(v = c(quantile(xbars.sim, 0.025), quantile(xbars.sim, 0.975)), col = "red")
abline(v = xbar, col = "blue")




