library(MASS)

N <- 10000
m <- rlnorm(N, 1, 1)
n <- rpois(N, m)

fit <- fitdistr(n, 'negative binomial')

par(mfcol=c(2, 1), mar=rep(0.1, 4), oma=c(4, 4, 1, 1))
plot(ecdf(n)(1:max(n)), 
     log='xy', xaxt='n', 
     ylim=c(pnbinom(1, size=fit$estimate['size'], mu=fit$estimate['mu']), 1))
points(pnbinom(1:max(n), size=fit$estimate['size'], mu=fit$estimate['mu']), 
       col='red', type='b', cex=0.5, pch=16)
plot(dnbinom(1:max(n), size=fit$estimate['size'], mu=fit$estimate['mu']), 
     col='red', type='b', cex=0.5, pch=16, log='x')
