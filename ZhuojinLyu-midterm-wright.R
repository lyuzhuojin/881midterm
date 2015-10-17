#Guide:

#First, several functions illustrate how every part of the overall function operates
#and present the relationship among exponential, poisson and gamma distribution.
#Then, I check each function below and throw several expected errors.
#Finally, some suggestions have offered.

library(ggplot2)
library(qualityTools)



#1.The following three functions tell us the relationship between poisson and exponential distribution

#-----------------------------------------------------------------------------------------------------
# Assume the waiting time for occurence of events has exponential distribution
# wait() returns a vector of w exponential waiting times with lambda = lam

wait <- function(w,lam){
# Arguments: w   positive integer. the number of waiting times;
#           lam  positive numeric. the parameter of exponential distribution; 

  if(is.integer(w) == FALSE) stop("w should be integer")
  if((w > 0) == FALSE) stop("w should be positive")
  if(is.numeric(lam)  == FALSE) stop("lam should be integer")
  if((lam > 0) == FALSE) stop("lam should be positive")
  a = NULL
  for(i in 1:w){
    a = c(a,rexp(1,rate = lam))
  }
  return(a)
}


#check wait()
set.seed(50)
exp <- wait(100L,4)
qplot(exp, binwidth = 0.1, geom = "density", xlab = "", ylab = "exponential density")

wait(-1L,-4.5)

#Suggestion1: In my opinion, wait() only helps us to understand exponential distribution and its meaning,
#so I think we can ignore it.

#--------------------------------------------------------------------------------------------------------
# wait.until() returns a vector of exponential waiting times whose total t <= Max with lambda = lam

wait.until <- function(Max,lam){
# Arguments: Max  positive numeric.  the total time of waiting times;  
#            lam  positive numeric. the parameter of exponential distribution; 
  if(is.numeric(Max) == FALSE)stop("Max should be numeric")
  if((Max > 0) == FALSE)stop("Max should be positive")
  if(is.numeric(lam) == FALSE)stop("lam should be integer")
  if((lam > 0) == FALSE)stop("lam should be positive")
  i <- 1
  inter <- 0
  while(sum(inter) <= Max){
    inter[i] <- rexp(1,lam)
    i <- i+1
  }
  return(inter[1:(length(inter)-1)])  
}

#check
set.seed(50)
e1 <- wait.until(100,4)
qplot(e1, binwidth = 0.1, geom = "density",xlab = "exponential waiting times")
wait.until(-100,4)
wait.until(100,-4)
wait.until(100,"4")

#----------------------------------------------------------------------------------------------------------
# poi.test() simulates the number of events, which has poisson distribution
# poi.test() is created according to the number of events divided by exponential waiting times 
# are Poisson distributed

poi.test <- function(rep, max, lam){
# Arguments: rep  positive interger.  the size of simulated poisson sample; 
#            max  positive numeric.  the total time of waiting times;   
#            lam  positive numeric. the parameter of exponential distribution; 

  if(is.integer(rep) == FALSE)stop("rep should be integer")
  if((rep > 0) == FALSE)stop("rep should be positive")
  if(is.numeric(max) == FALSE)stop("max should be numeric")
  if((max > 0) == FALSE)stop("max should be positive")
  if(is.numeric(lam) == FALSE) stop("lam should be numeric")
  if((lam >0) == FALSE) stop("lam should be positive")
  b <- 0
  for(i in 1:rep){
    b[i] = length(wait.until(max,lam))
  }
  return(b)
}

#check
set.seed(50)
a <- poi.test(1000L,1,4)
mean(a)
var(a)
hist(a, xlab = "",ylab = "the number of events", main = "simulated poisson distribution")
rp <- rpois(1000,4)
ks.test(a,rp)
#According to p-value of Kolmogorov-Smirnov Test, the simulated distribution
#is poisson distribution






# ***************** Comments from yulan *****************:
# When I run it, the p-value of ks test is less than 0.8.
# Do you think it's ok to change lambda to mean(a) instead of 4?
rp <- rpois(1000, mean(a))
ks.test(a,rp)






# ***************** Comments from yulan ******************:
# Maybe here we can also draw the real density of poisson distribution and compare it with density of a?
# It's a rough way to check distribution.
scale <- length(a)
x <- seq(min(a),max(a),1)
y <- dpois(x,4)*scale
qplot(a, binwidth = 1) + geom_line(aes(x,y,color='red'))





#2.The following three functions tell us the relationship between gamma and exponential distribution

#--------------------------------------------------------------------------------------------------------
# Assume the waiting time for occurence of events has exponential distribution
# wait.for() simulates the waiting time until occurence of the kth event with lambda = lam

wait.for <- function(k, lam){
# Arguments: k  positive integer.  the number of events;
#            lam  positive numeric. the parameter of exponential distribution; 
  if(is.integer(k) == FALSE) stop("k should be integer")
  if((k > 0) == FALSE) stop("k should be positive")
  if(is.numeric(lam) == FALSE) stop("lam should be numeric")
  if((lam >0) == FALSE) stop("lam should be positive")
  time = 0
  count = 0
  a = NULL
  while(count < k){
    inter=rexp(1,lam)
    count = count + 1
    time = time+inter
  }
  
  return(time)
} 


#check
set.seed(50)
wait.for(10L,4)
wait.for(-10L,4)
wait.for(10L,-4)

#------------------------------------------------------------------------------------------------------
# gam.test() simulates gamma distribution
# gam.test() is created according to the arrival time in the poisson process is gamma function

gam.test <-function(rep, K, lam ){
# Arguments: rep  interger.  the size of simulated gamma sample; rep should be positive
#            K  integer.  the number of events, K should be positive
#            lam  numeric. the parameter of exponential distribution; 
#            lam should be bigger than zero
  if((is.integer(rep)) == FALSE)stop("rep should be integer")
  if((rep > 0) == FALSE)stop("rep should be positive")
  if(is.integer(K) == FALSE) stop("max.e should be integer")
  if((K > 0) == FALSE) stop("max.e should be positive")
  if(is.numeric(lam) == FALSE) stop("lam should be numeric")
  if((lam >0) == FALSE) stop("lam should be positive")
  a=NULL
  for (i in 1:rep){
    t = wait.for(K,lam)
    a = c(a,t)
  }
  return(a)
}

#check
c <- gam.test(1000L,2L,10)
qplot(c, geom = "density", binwidth = 0.1, xlab = "the time until K arrival", main = "simulated gamma distribution")
#when alpha = 2, beta = 1/10, then the gamma function is shown below
t <- seq(0,1,0.01)
ft <- 100*t*exp(-10*t)
plot(t,ft,type="l", xlab="T", ylab="f(t)")+
title(main=expression(paste("Probability density function of ",
                            Gamma(2,frac(1,10)))))
g <- rgamma(1000,2,10)
ks.test(c,g)
#According to p-value of Kolmogorov-Smirnov Test, the simulated distribution
#is gamma distribution

#Conclusion:
#1. If X has poisson process with parameter lambda, 
#   the time until the first arrival is exponential distribution
#2. If X has poisson process with parameter lambda,
#   the time until kth arrival is gamma distribution

#Suggestion: If we just want to get samples of exponential, poisson and gamma distributions, 
#we can use built-in function rexp(n, lambda), rpois(n, lambda), rgamma(n, alpha, beta)

  
  
  
  
  
  

