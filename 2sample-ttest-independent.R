# Is there a meaningfull difference between the average exercise time of
# male or female MBA students. 

# H0: mu1 - mu2 = 0
# Ha: mu1 - mu2 neq 0

data <- read.csv("http://tiny.cc/fa18classData")
names(data)

# Need to filter data in male or female. Later, I show you a quicker way to do it.
data.m = data[data$gender=="Male",]
data.f = data[data$gender=="Female",]

# null Hypothesis
null.diff = 0

# Sample 1: Male students
n1 = length(data.m$excerise.time); n1
xbar1 = mean(data.m$excerise.time); xbar1
s1 = sd(data.m$excerise.time); s1
# Sample 2: Female students
n2 = length(data.f$excerise.time); n2
xbar2 = mean(data.f$excerise.time); xbar2
s2= sd(data.f$excerise.time); s2 
# Mean times are very close to each other.
# Is this just happened by chance? Or if we repeat sampling we expect same results.

# The null distribution of (xbar1-xbar2) can be approximated by t if:
## populations are normal, which we don't have any clue about that.
## Or n1 and n2 are large enough.
library(e1071)
kurt1 = kurtosis(data.m$excerise.time) 
kurt2 = kurtosis(data.f$excerise.time)
skew1 = skewness(data.m$excerise.time) 
skew2 = skewness(data.f$excerise.time)
n1 > max(10*skew1^2, 10*abs(kurt1)) 
n2 > max(10*skew2^2, 10*abs(kurt2))

# Lets start performing a t test
xbar.diff = xbar1-xbar2

# find test Statistic 
Tstat = (xbar.diff-null.diff)/sqrt(s1^2/n1 + s2^2/n2) 
Tstat

# You can use this function for finding the degree of freedom for 2sample t test
df.t <- function(s1, s2, n1, n2){
  nom = (s1^2/n1 + s2^2/n2)^2
  denom = (s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1)
  nom/denom
}

df = df.t(s1,s2,n1,n2)
df
# Is Zstat falls in the the critical region? Since it's one-sided test:
alpha = 0.05
qt(1-alpha/2, df)
Tstat >  qt(1-alpha/2, df) # Check upper tail
Tstat < -qt(1-alpha/2, df) # Check lower tail

# finding p-value
# pvalue = 2*P(T > |Tstat|) = 2*(1 - P(T <= Tstat))
2*(1-pt(abs(Tstat), df)) 


# How to do it in R:
t.test(data.m$excerise.time, data.f$excerise.time)
# You apply t.test on nonfiltered data as follow:
attach(data)
t.test(excerise.time ~ gender)


# How to understand this by simulation:
# Simulate the null distribution:
set.seed(123)
nsim = 10000
ntot1 = n1*nsim
ntot2 = n2*nsim
rv1 = rnorm(ntot1, 0, s1)  
rv2 = rnorm(ntot2, 0, s2) # Why mean is zero for both?
rvm1 = matrix(rv1, nrow = nsim)
rvm2 = matrix(rv2, nrow = nsim)
hist(xbars.diff, freq=F, main ="simulated H0 Histogram")
abline(v = c(quantile(xbars.diff, alpha/2), quantile(xbars.diff, 1-alpha/2)), col="red")
abline(v = xbar.diff, col = "blue")


