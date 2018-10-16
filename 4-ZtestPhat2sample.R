# H0: p1 - p2 <= 0.04
# Ha: p1 - p2  >  0.04

# null Hypothesis
null.diff = 0.04

# Sampling information:
n1 = 150
phat1 = 0.72 # Review of 150 members using 
## Atkin's diet showed 72% renewed their membership.
n2 = 220
phat2 = 0.60 # Review of 220 members using 
## regular diet showed 60% renewed their membership.
# phat1 is 12% more than phat2.
# Is this just happened by chance?

# The null distribution of (phat1-phat2) can be approximated by normal dist if  
n*phat1 > 10; n*(1-phat1) > 10
n*phat2 > 10; n*(1-phat2) > 10


# The null distribution of (phat1-phat2) has following parametrs:
muPhat.diff = 0.04
sigmaPhat.diff = sqrt(phat1*(1-phat1)/n1 + phat2*(1-phat2)/n2) 
# which we call it SE of phat

# find test Statistic 
Zstat = (phat1-phat2-null.diff)/sqrt(phat1*(1-phat1)/n1 + phat2*(1-phat2)/n2) 
Zstat

# Is Zstat falls in the the critical region? Since it's one-sided test:
alpha = 0.05
qnorm(1-alpha)
Zstat > qnorm(1-alpha)
# Fail to reject null hypthesis at 0.05 level of significance. 
# That means the difference between phat1 and phat2 
# is not significantly more than 0.04.

# What happened graphically:
z = seq(-4, 4, 0.01) # making sequnce of numbers between -4 and 4
plot(z, dnorm(z), main = "H0 distribution", type = "l", yaxs="i")
abline(v = qnorm(1-alpha)) # Critical value
abline(v = Zstat , col = "red") # Zstat

# Closer look
plot(z, dnorm(z), main = "H0 distribution", type = "l", yaxs="i", xlim=range(0, 3))
abline(v = qnorm(1-alpha)) # Critical value
abline(v = Zstat , col = "red") # Zstat


# finding p-value
# pvalue = P(z > zstat) = 1 - P(z > zstat)
1-pnorm(Zstat)


# How to understand this by simulation:
# Simulate the null distribution:
nsim = 10000
ntot1 = n1*nsim
ntot2 = n2*nsim
rv1 = sample(0:1, ntot1, p = c(0.36, 0.64), replace = T)
rv2 = sample(0:1, ntot2, p = c(0.40, 0.60), replace = T)
rvm1 = matrix(rv1, nrow = nsim)
rvm2 = matrix(rv2, nrow = nsim)
phats1 = rowMeans(rvm1)
phats2 = rowMeans(rvm2)
phats.diff = phats1 - phats2
hist(phats.diff, freq=F, main ="simulated H0 Histogram")
abline(v = quantile(phats.diff, 0.95))
abline(v = phat1-phat2, col = "red")


