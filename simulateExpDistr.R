library(ggplot2)
simulateExpDistr<-function(nosim, observations, lambda)
{
    observationDF<-replicate(nosim, rexp(observations,lambda))
    
    means <- cumsum(colMeans(observationDF))/(1:nosim)    
    plot(1:nosim, means, type = "l", lwd = 2, frame = FALSE, ylab = "cumulative means",  xlab = "sample size") 
    abline(h = 1/lambda)
    
    vars <- cumsum(apply(observationDF, 2, var))/(1:nosim) 
    plot(1:nosim, vars, type = "l", lwd = 2, frame = FALSE, ylab = "cumulative variance",  xlab = "sample size") 
    abline(h = (1/lambda)^2)
    
    sds <- cumsum(apply(observationDF, 2, sd))/(1:nosim) 
    plot(1:nosim, sds, type = "l", lwd = 2, frame = FALSE, ylab = "cumulative standard deviations",  xlab = "sample size") 
    abline(h = 1/lambda)
    
    cfunc <- function(x, n) (mean(x) - 1/lambda) / ((1/lambda)/(sqrt(n)))
     dat <- data.frame(
          x = c(apply(observationDF, 2, cfunc, observations)),
          size = factor(c(observations), rep(nosim))
     )
    g <- ggplot(dat, aes(x = x)) + geom_histogram(alpha = .20, binwidth=.3, colour = "black", fill="green", aes(y = ..density..)) 
    g <- g + stat_function(fun = dnorm, size = 2)
    print(g)
   
   error<-mean(sds)/sqrt(observations)
   ci<-mean(means)+c(-1,1)*1.96*error
   return (ci)
}
