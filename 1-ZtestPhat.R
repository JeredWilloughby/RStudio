# H0: p >= 0.2
# Ha: p < 0.2

# null Hypothesis
p0 = 0.2

# Sampling information:
n = 100 
phat = 0.12 # Review of 100 emails showed 12% spam.
# Less than 20% spams passed through which is a good news.
# Is this just happened by chance?

# The null distribution of phat can be approximated by normal dist if  
n*p0 > 10
n*(1-0) > 10

# The null distribution of phat has following parametrs:
muPhat = p0
sigmaPhat = sqrt(p0*(1-p0)/n) # which we call it SE of phat

# find test Statistic (Z score which is always equal to (x-mu)/sigma)
Zstat = (phat-p0)/sqrt(p0*(1-p0)/n)
Zstat

# Is Zstat falls in the the critical region? Since it's one-sided test:
alpha = 0.05
Zstat < -qnorm(1-alpha)

# What happened graphically:
z = seq(-4, 4, 0.01) # making sequnce of numbers between -4 and 4
plot(z, dnorm(z), main = "H0 distribution", type = "l", yaxs="i")
abline(v = -qnorm(1-alpha)) # Critical value
abline(v = Zstat , col = "red") # Zstat

# finding p-value
# pvalue = P(z < zstat)
pnorm(Zstat)


# How to understand this by simulation:
# Simulate the null distribution:
nsim = 10000
ntot = n*nsim
rv = sample(0:1, ntot, p = c(0.8, 0.2), replace = T)
rvm = matrix(rv, nrow = nsim)
phats = rowMeans(rvm)
hist(phats, freq=F, main ="simulated H0 Histogram")
abline(v = phat, col = "red")
mean(phats < phat)

phats = rnorm(nsim, muPhat, sigmaPhat)
hist(phats, freq=F, main ="simulated H0 Histogram")
abline(v = phat, col = "red")
mean(phats < phat)












