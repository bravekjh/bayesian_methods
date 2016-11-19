

# Starting Homework 7

# Question 8.3 

## Bring in the libraries
library("plyr")
library("mcmcse")

### Import the data

numfiles <- 8
path.intro <- "C:/Users/Philip/Schools/TAMU/STAT_638/Homework_Assignments/HW_07/"

filenames <- list.files(path = path.intro, pattern = "*.dat")
filenames.path <- paste(path.intro, filenames, sep="")
list_of_data <- llply(filenames.path, read.table, colClasses = c("numeric"))

school.input <- list(
                c(length(list_of_data[[1]]$V1), mean(list_of_data[[1]]$V1), var(list_of_data[[1]]$V1) )
                , c(length(list_of_data[[2]]$V1), mean(list_of_data[[2]]$V1), var(list_of_data[[2]]$V1) )
                , c(length(list_of_data[[3]]$V1), mean(list_of_data[[3]]$V1), var(list_of_data[[3]]$V1) )
                , c(length(list_of_data[[4]]$V1), mean(list_of_data[[4]]$V1), var(list_of_data[[4]]$V1) )
                , c(length(list_of_data[[5]]$V1), mean(list_of_data[[5]]$V1), var(list_of_data[[5]]$V1) )
                , c(length(list_of_data[[6]]$V1), mean(list_of_data[[6]]$V1), var(list_of_data[[6]]$V1) )
                , c(length(list_of_data[[7]]$V1), mean(list_of_data[[7]]$V1), var(list_of_data[[7]]$V1) )
                , c(length(list_of_data[[8]]$V1), mean(list_of_data[[8]]$V1), var(list_of_data[[8]]$V1) )
                )

# Take a look
school.input

# Assign the required priors
mu.0 <- 7; gamma.0 <- 5; tau.0.sq <- 10; eta.0 <- 2; sigma.0.sq <- 15; nu.0 <- 2

# Write in function for generation of normal hierarchical model

normal.hierarchy.suff=function(Y,nreps,mu0,gamma0,eta0,tau0,nu0,sigma0){
  #
  #
  # This function provides Gibbs samples from the posterior of the
  # normal hierarchical model on pp. 184-186 of the notes.  The priors are
  # the same as in the notes.  The input Y is a list with m components.
  # Each component is a three-vector containing sufficient statistics for
  # one group: the sample size, the sample mean and the sample variance
  # (in that order).  It is assumed that the sample variance uses n-1 in 
  # the denominator.  
  #
  #  
  m=length(Y)  
  theta=matrix(0,nreps+1,m)  
  sigma2=1:(nreps+1)  
  mu=1:(nreps+1)  
  tau2=1:(nreps+1)  
  n=1:m  
  ybar=1:m  
  s2=0  
  for(j in 1:m){    
    n[j]=Y[[j]][1]    
    theta[1,j]=Y[[j]][2]    
    ybar[j]=theta[1,j]    
    s2=s2+(n[j]-1)*Y[[j]][3]  
  }  
  N=sum(n)  
  sigma2[1]=s2/N  
  mu[1]=sum(n*ybar)/N  
  tau2[1]=s2/mean(n)  
  for(i in 2:(nreps+1)){    
    par=s2+sum(n*(ybar-theta[i-1,])^2)    
    par=(par+nu0*sigma0^2)/2    
    sigma2[i]=1/rgamma(1,(N+nu0)/2,rate=par)    
    par=(sum((theta[i-1,]-mu[i-1])^2)+eta0*tau0^2)/2    
    tau2[i]=1/rgamma(1,(m+eta0)/2,rate=par)    
    mutilde=sum(theta[i-1,])/tau2[i]+mu0/gamma0^2    
    mutilde=mutilde/(m/tau2[i]+1/gamma0^2)    
    sigmatilde=(m/tau2[i]+1/gamma0^2)^(-1/2)    
    mu[i]=rnorm(1,mutilde,sigmatilde)    
    vecmean=(n*ybar/sigma2[i]+mu[i]/tau2[i])/(n/sigma2[i]+1/tau2[i])    
    vecsd=sqrt(1/(n/sigma2[i]+1/tau2[i]))    
    theta[i,]=rnorm(m,vecmean,vecsd)  
  }
  list(theta[2:(nreps+1),],sigma2[2:(nreps+1)],mu[2:(nreps+1)],tau2[2:(nreps+1)])
}

# call the function
school.output <- normal.hierarchy.suff(school.input
                      , nreps = 2000
                      , mu0 = mu.0
                      , gamma0 = gamma.0
                      , eta0 = eta.0
                      , tau0 = tau.0.sq
                      , nu0 = nu.0
                      , sigma0 = sigma.0.sq
                      )

# Now that we have run our Markov chain, let's start answering some questions
# The order of the school.output list is
##theta
##sigma2
##mu
##tau2


# 8.3.a)

## Assess the convergence of the Markov chain
## First let's plot the output of each parameter chain for each School 

# theta
par(mfrow=c(2,4))
plot(school.output[[1]][,1])
plot(school.output[[1]][,2])
plot(school.output[[1]][,3])
plot(school.output[[1]][,4])
plot(school.output[[1]][,5])
plot(school.output[[1]][,6])
plot(school.output[[1]][,7])
plot(school.output[[1]][,8])

# sigma2
par(mfrow=c(1,1))
plot(school.output[[2]])

# mu
par(mfrow=c(1,1))
plot(school.output[[3]])

# tau2
par(mfrow=c(1,1))
plot(school.output[[4]])

## Using the ess function from the mcmcse package

ess(x = as.matrix((school.output[[2]])))
ess(x = as.matrix((school.output[[3]])))
ess(x = as.matrix((school.output[[4]])))

      
# 8.3.b)
# Compute posterior means and 95% conf. regions for sigma2, mu, tau2

# sigma2
avg <- mean(school.output[[2]])
one <- mean(school.output[[2]]) - 1.96*sqrt(var(school.output[[2]]))/sqrt(length(school.output[[2]]))
two <- mean(school.output[[2]]) + 1.96*sqrt(var(school.output[[2]]))/sqrt(length(school.output[[2]]))
# display
c(one, avg, two)
plot(density(school.output[[2]]))
abline(v = avg, col="blue")

# mu
avg <- mean(school.output[[3]])
one <- mean(school.output[[3]]) - 1.96*sqrt(var(school.output[[3]]))/sqrt(length(school.output[[3]]))
two <- mean(school.output[[3]]) + 1.96*sqrt(var(school.output[[3]]))/sqrt(length(school.output[[3]]))
# display
c(one, avg, two)
plot(density(school.output[[3]]))
abline(v = avg, col="blue")

# tau2
avg <- mean(school.output[[4]])
one <- mean(school.output[[4]]) - 1.96*sqrt(var(school.output[[4]]))/sqrt(length(school.output[[4]]))
two <- mean(school.output[[4]]) + 1.96*sqrt(var(school.output[[4]]))/sqrt(length(school.output[[4]]))
# display
c(one, avg, two)
plot(density(school.output[[4]]))
abline(v = avg, col="blue")


# 8.3.c




tau.2.post <- school.output[[4]]
sigma.2.post <- school.output[[2]]

big.r.post <- tau.2.post / (sigma.2.post + tau.2.post)


# 8.4.d
## obtain the postioer probability that theta.7 < theta.6

mean(school.output[[1]][,6] > school.output[[1]][,7])
plot(density(school.output[[1]][,6]))
lines(density(school.output[[1]][,7]), lty = 2)

prob <- (
mean(school.output[[1]][,7] < school.output[[1]][,1])*
mean(school.output[[1]][,7] < school.output[[1]][,2])*
mean(school.output[[1]][,7] < school.output[[1]][,3])*
mean(school.output[[1]][,7] < school.output[[1]][,4])*
mean(school.output[[1]][,7] < school.output[[1]][,5])*
mean(school.output[[1]][,7] < school.output[[1]][,6])*
mean(school.output[[1]][,7] < school.output[[1]][,8])
)

# Display the result
prob

# 8.3.e
## Plot the sample averages against against
## the posterior expectations and describe the relationship between them

par(mfrow=c(2,4))

# School 1
plot(density(school.output[[1]][,1]))
lines(density(list_of_data[[1]]$V1))
c(mean(list_of_data[[1]]$V1),mean(school.output[[1]][,1]))

# School 2
plot(density(school.output[[1]][,2]))
lines(density(list_of_data[[2]]$V1))
c(mean(list_of_data[[2]]$V1),mean(school.output[[1]][,2]))

# School 3
plot(density(school.output[[1]][,3]))
lines(density(list_of_data[[3]]$V1))
c(mean(list_of_data[[3]]$V1),mean(school.output[[1]][,3]))

# School 4
plot(density(school.output[[1]][,4]))
lines(density(list_of_data[[4]]$V1))
c(mean(list_of_data[[4]]$V1),mean(school.output[[1]][,4]))

# School 5
plot(density(school.output[[1]][,5]))
lines(density(list_of_data[[5]]$V1))
c(mean(list_of_data[[5]]$V1),mean(school.output[[1]][,5]))

# School 6
plot(density(school.output[[1]][,6]))
lines(density(list_of_data[[6]]$V1))
c(mean(list_of_data[[6]]$V1),mean(school.output[[1]][,6]))

# School 7
plot(density(school.output[[1]][,7]))
lines(density(list_of_data[[7]]$V1))
c(mean(list_of_data[[7]]$V1),mean(school.output[[1]][,7]))

# School 8
plot(density(school.output[[1]][,8]))
lines(density(list_of_data[[8]]$V1))
c(mean(list_of_data[[8]]$V1),mean(school.output[[1]][,8]))

# Now, compute the sample mean of all obs and compare it
# to the posterior mean of mu

summation.2 <- 0
lengthiness.2 <- 0

for(i in 1:numfiles)
{
summation <- sum(list_of_data[[i]]$V1)
summation.2 <- rbind(summation.2, summation)

lengthiness <- length(list_of_data[[i]]$V1)
lengthiness.2 <- rbind(lengthiness.2, lengthiness)
}

sample.mean <- sum(summation.2) / sum(lengthiness.2)

# Now for the posterior mean

summation.2 <- 0
lengthiness.2 <- 0

for(i in 1:numfiles)
{
  summation <- sum(school.output[[1]][,i])
  summation.2 <- rbind(summation.2, summation)

  lengthiness <- length(school.output[[1]][,i])
  lengthiness.2 <- rbind(lengthiness.2, lengthiness)
}

posterior.mean <- sum(summation.2) / sum(lengthiness.2)

c(sample.mean, posterior.mean)


