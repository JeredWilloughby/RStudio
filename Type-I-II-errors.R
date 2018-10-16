# Average speed in Marsha Sharp freeway:
# H0: mu = 65
# Ha: mu > 65 

mu0 = 65
sigma = 6 #sigma is known

# Type I Error. We determine it:
alpha = 0.05 

# But imagine the true mean is different.
mu.true = 68

# sample size:
n = 30
se = sigma/sqrt(n)
# What happened graphically:
xbar = seq(62, 72, 0.1) # making sequnce of possible numbers
plot(xbar, dnorm(xbar, mu0, se), main = "Distribution of xbar", type = "l", yaxs="i")
cv = qnorm(1-alpha, mu0, se)# Non-Standardized Critical Value 
abline(v= cv, col = "red") 
points(xbar, dnorm(xbar, mu.true, se), type = "l", lty=2, yaxs="i")
legend(70, .30, legend=c("Null Dist", "True Dist"),
       lty=1:2, cex=0.8)
# beta: type II error is probability of not rejecting H0 whil it is not true
# The left tail area under the dashed line curve
beta = pnorm(cv, mu.true, se) 
beta

power = 1 - beta
power

# Change alpha, n and sigma and see the impact on beta



## Try simulation:
# First, simulate the null distribution:
set.seed(123)
nsim = 10000
ntot = n*nsim
rv = rnorm(ntot, mu0, sigma) 
rvm = matrix(rv, nrow = nsim)
xbars.null = rowMeans(rvm)
hist(xbars.null, freq=F, main ="Null Histogram for Xbars")
cv.null = quantile(xbars.null, 1-alpha)
abline(v = cv.null, col = "red")

# Then simulate the true distribution:
rv.true = rnorm(ntot, mu.true, sigma)
rvm.true = matrix(rv.true, nrow = nsim)
xbars.true = rowMeans(rvm.true)
hist(xbars.true, freq=F, main ="True Histogram for Xbars")
abline(v = cv.null, col = "red")
# Type II Error by simulation:
beta = mean(xbars.true < cv.null)
beta