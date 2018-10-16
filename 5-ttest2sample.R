# H0: mu1 - mu2 <= 5
# Ha: mu1 - mu2  > 5

# null Hypothesis
null.diff = 5

# Sampling information:
n1 = 33
xbar1 = 15.42
s1 = 14.37 # Review of 33 members using 
## Atkin's diet showed on average they lost 15.42 lb with sdev = 14.37 lb.
n2 = 30
xbar2 = 7
s2= 12.36 # Review of 30 members using 
## regular diet showed on average they lost 7 lb with sdev = 12.36 lb.
# Xbar1 is 8.42 lb more than xbar2.
# Is this just happened by chance?

# The null distribution of (xbar1-xbar2) can be approximated by t dist if  
kurt1 = 0.1; kurt2 = -0.565 
n1 > 10*abs(kurt1); n > 10*abs(kurt2)

xbar.diff = xbar1-xbar2

# The null distribution of xbar.diff has following parametrs:
muXbar.diff = null.diff
sigmaXbar.diff = sqrt(s1^2/n1 + s2^2/n2) 
# which we call it SE of xbar.diff

# find test Statistic 
Tstat = (xbar.diff-null.diff)/sqrt(s1^2/n1 + s2^2/n2) 
Tstat

df.t <- function(s1, s2, n1, n2){
  nom = (s1^2/n1 + s2^2/n2)^2
  denom = (s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1)
  nom/denom
}

df = df.t(s1,s2,n1,n2)
df
# Is Zstat falls in the the critical region? Since it's one-sided test:
alpha = 0.05
qt(1-alpha, df)
Tstat > qt(1-alpha, df)
# Fail to reject null hypthesis at 0.05 level of significance. 
# That means the difference between xbar1 and xbar2 
# is not significantly more than 5 lb.

# What happened graphically:
t = seq(-4, 4, 0.01) # making sequnce of numbers between -4 and 4
plot(t, dt(t, df), main = "H0 distribution", type = "l", yaxs="i")
abline(v = qt(1-alpha, df)) # Critical value
abline(v = Tstat , col = "red") # Zstat


# finding p-value
# pvalue = P(T > Tstat) = 1 - P(T <= Tstat)
1-pt(Tstat, df)


# How to understand this by simulation:
# Simulate the null distribution:
nsim = 10000
ntot1 = n1*nsim
ntot2 = n2*nsim
rv1 = rnorm(ntot1, 12, s1) 
rv2 = rnorm(ntot2, 7, s2)
rvm1 = matrix(rv1, nrow = nsim)
rvm2 = matrix(rv2, nrow = nsim)
xbars.diff = rowMeans(rvm1)-rowMeans(rvm2)
hist(xbars.diff, freq=F, main ="simulated H0 Histogram")
abline(v = quantile(xbars.diff, 0.95))
abline(v = xbar.diff, col = "red")


