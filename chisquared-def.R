## The Chi Squared Distribution as a sum of squared Z values

df = 10

## The density function of the chi squared distribution 
## with 10 degrees of freedom is shown below.

chi.values = seq(0,3*df,length.out=10000)
chi.dens = dchisq(chi.values,df)

plot(chi.values, chi.dens, type="l", 
     main=paste0("Chi Square Distribution for the Sum of ",
                 df, " Squared Z's"), xlab="Chi-Squared Value", 
     ylab="density", yaxs="i", ylim = c(0, 1.2*max(chi.dens) ))
abline(v = df, lwd=2)


## The distribution of the sum of squared z-values:

NSIM = 10000
df = 10
z = rnorm(df*NSIM,0,1)
z = matrix(z, nrow=NSIM,ncol=df)
chisq = rowSums(z^2)

hist(chisq, freq=F, breaks=100)
abline(v=df, col = "green")


