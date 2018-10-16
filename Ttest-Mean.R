
# H0: mu <= $1500
# Ha: mu > $1500 

# null Hypothesis
mu0 = 1500

# Sampling information:
n = 115
xbar = 1657 # Review of 115 rental properties showed that
# the average rent is $1675 with s = $581.
s = 581
# 
# Sample average is >$1500, is this just happened by chance?

# The null distribution of xbar can be approximated by t dist

# The null distribution of xbar has following parametrs:
muXbar = mu0
sigmaXbar = s/sqrt(n) # which we call it SE of Xbar

# find test Statistic
T0 = (xbar-mu0)/(s/sqrt(n))
T0

# Check the significance. 
alpha = 0.05
df = n-1
# Should you check left tail or the right tail?
T0 > qt(1-alpha, df)
# Reject the null hypthesis. That means the difference 
# between xbar and mu0 is not by chance.

# What happened graphically:
t = seq(-4, 4, 0.01) # making sequnce of numbers between -4 and 4
plot(t, dt(t, df), main = "H0 distribution", type = "l", yaxs="i")
abline(v = qt(1-alpha, df), col = "red") # Critical value
abline(v = T0 , col = "blue") # Zstat

# finding p-value
# pvalue = P(T > Tstat) = 1 - P(T <= Tstat)
1-pt(Tstat, df)

# How to understand this by simulation:
# Simulate the null distribution:
nsim = 10000
ntot = n*nsim
rv = rnorm(ntot, mu0, s)
rvm = matrix(rv, nrow = nsim)
xbars.sim = rowMeans(rvm)
hist(xbars.sim, freq=F, main ="simulated H0 Histogram")
abline(v = quantile(xbars.sim, 0.95), col = "red")
abline(v = xbar, col = "blue")




