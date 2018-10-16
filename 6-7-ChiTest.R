data <- read.csv("http://tiny.cc/amazonStat")

head(data)

data$incomeLevel = c("L1:25orLess", "L2:26to50", 
                     "L3:51to75", "L4:76to100", "L5:100orMore")[
                       findInterval(data$income, c(-Inf, 25000, 50000, 
                                                   75000, 100000, Inf))]
head(data)

# H0: Income and Product category are independent of each other
# Ha: Income and Product category are not independent of each other

# Number of Observations
n = length(data$income)
n

# Creating contingency table:
obs.table <- table(data$category, data$incomeLevel)
obs.table

mosaicplot(t(obs.table), col = 2:3)

# Creating the expected null hypothesis table. 
# A table that rows and columns are independet of each other.
rowsum = rowSums(obs.table)
colsum = colSums(obs.table)
exp.table <- obs.table
for (i in 1:nrow(obs.table)) {
  for (j in 1:ncol(obs.table)) {
    exp.table[i,j] = rowsum[i]*colsum[j]/n
  }
}
exp.table
mosaicplot(t(exp.table), col = 2:3)

# Finding Chi Square Test Statistic
X2stat = sum((obs.table-exp.table)^2/exp.table)
X2stat

# Critical value:
df = (nrow(obs.table)-1)*(ncol(obs.table)-1)
qchisq(1-alpha, 4)

# If Chi Square Test Statistic drops in the region of rejection, we reject H0:
X2stat > qchisq(1-alpha, 4)
# So income and product caregory are not independent of each other.

# What happened graphically:
x = seq(0, 40, 0.01) # making sequnce of numbers between -4 and 4
plot(x, dchisq(x, df), main = "H0 distribution", type = "l", yaxs="i")
abline(v = qchisq(1-alpha, 4)) # Critical value
abline(v = X2stat , col = "red") # Test stat

# finding p-value
# pvalue = P(X2 > X2stat) = 1 - P(X2 <= X2stat)
1-pchisq(X2stat, df)

# Do everything quickly by chisq.test function:
test = chisq.test(obs.table) 
test

