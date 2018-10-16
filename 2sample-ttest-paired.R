# Paired Test

# H0: muD  =  0
# Ha: muD neq 0

# null Hypothesis 
D0 = 0

type1 = c(53.6, 103.4, 68.2, 88.4, 111.6)
type2 = c(61.4, 112.8, 67.1, 92.3, 121.5)
cor(type1, type2)

diff = type1-type2 
# Sample information
n = length(diff); n
Dbar = mean(diff); Dbar
sD = sd(diff); sD
# Is this diffrence just happened by chance? 
# Or if we repeat sampling we expect to see differences again.

# find test Statistic 
Tstat = (Dbar-D0)/(sD/sqrt(n)) 
Tstat

df = n-1
df
# Is Tstat falls in the the critical region? Since it's one-sided test:
alpha = 0.05
qt(1-alpha/2, df)
Tstat >  qt(1-alpha/2, df) # Check upper tail
Tstat < -qt(1-alpha/2, df) # Check lower tail

# finding p-value
# pvalue = 2*P(T > |Tstat|) = 2*(1 - P(T <= Tstat))
2*(1-pt(abs(Tstat), df)) 


# R function
t.test(type1, type2, paired = TRUE)




