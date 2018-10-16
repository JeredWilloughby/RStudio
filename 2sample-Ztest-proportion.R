# Is there a meaningfull difference between the 
# proportion of male and female students who have breakfast in the morning

# H0: p1 - p2 = 0
# Ha: p1 - p2 neq 0

pnull.diff = 0

data <- read.csv("http://tiny.cc/fa18classData")
names(data)

t = table(data$today.breakfast, data$gender); t

# phat1 = p(yes to breakfast |given female) = p(yes and female)/p(female)
# Same for phat2
p.t = prop.table(t, 2); p.t

phat1 = p.t[2,1]; phat1
phat2 = p.t[2,2]; phat2

agg = aggregate(today.breakfast~gender, FUN = length)
n1 = agg[1,2]; n1
n2 = agg[2,2]; n2

# find test Statistic 
Zstat = (phat1-phat2-pnull.diff)/sqrt(phat1*(1-phat1)/n1 + phat2*(1-phat2)/n2) 
Zstat

# Is Zstat falls in the the critical region? Since it's one-sided test:
alpha = 0.05
qnorm(1-alpha)
Zstat > qnorm(1-alpha/2)
Zstat < -qnorm(1-alpha/2)
# Fail to reject null hypthesis at 0.05 level of significance. 

# finding p-value for two-sided
# pvalue = 2*P(z > |zstat|) = 2*(1 - P(z > zstat))
2*(1-pnorm(abs(Zstat)))


# How to understand this by simulation:
# Simulate the null distribution:
nsim = 10000
ntot1 = n1*nsim
ntot2 = n2*nsim
rv1 = sample(0:1, ntot1, p = c(0.36, 0.64), replace = T)
rv2 = sample(0:1, ntot2, p = c(0.36, 0.64), replace = T)
rvm1 = matrix(rv1, nrow = nsim)
rvm2 = matrix(rv2, nrow = nsim)
phats1 = rowMeans(rvm1)
phats2 = rowMeans(rvm2)
phats.diff = phats1 - phats2
hist(phats.diff, freq=F, main ="simulated H0 Histogram")
abline(v = quantile(phats.diff, 0.975), col = "red")
abline(v = quantile(phats.diff, 0.025), col = "red")
abline(v = phat1-phat2, col = "blue")# from sample
