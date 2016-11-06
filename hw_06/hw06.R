

# Homework 06

# Question 7.3.a

# Read in the data

blue <- read.table("C:/Users/Philip/Schools/TAMU/STAT_638/Homework_Assignments/HW_06/bluecrab.dat")

orange <- read.table("C:/Users/Philip/Schools/TAMU/STAT_638/Homework_Assignments/HW_06/orangecrab.dat")

# specify functions for draws from multivariate normal and wishart distros

### sample from the multivariate normal distribution
rmvnorm<-function(n,mu,Sigma) 
{
  p<-length(mu)
  res<-matrix(0,nrow=n,ncol=p)
  if( n>0 & p>0 )
  {
    E<-matrix(rnorm(n*p),n,p)
    res<-t(  t(E%*%chol(Sigma)) +c(mu))
  }
  res
}
###


### sample from the Wishart distribution
rwish<-function(n,nu0,S0)
{
  sS0 <- chol(S0)
  S<-array( dim=c( dim(S0),n ) )
  for(i in 1:n)
  {
    Z <- matrix(rnorm(nu0 * dim(S0)[1]), nu0, dim(S0)[1]) %*% sS0
    S[,,i]<- t(Z)%*%Z
  }
  S[,,1:n]
}
###

## Blue crabs first
# Specify our starter parameters

mu0 <- apply(X = blue, MARGIN = 2, FUN = mean) # sample mean
L0 <- cov(blue) # sample covariance matrix
S0 <- cov(blue)
nu0 <- 4

# get a quick look at the data
plot(blue[,1] , blue[,2] , xlim=c(0,50), ylim=c(0,50))
lin.mod <- lm(Y[,2] ~ Y[,1] - 1)
summary(lin.mod)
abline(lin.mod)

n <- dim(blue)[1]
ybar <- apply(X = blue, MARGIN = 2, FUN = mean)
Sigma <- cov(blue)
THETA <- SIGMA <- NULL


set.seed(1738)
for(s in 1:5000)
{
  # generate a theta value
  Ln <- solve( solve(L0) + n*solve(Sigma)  )
  mun <- Ln%*%( solve(L0)%*%mu0 + n*solve(Sigma)%*%ybar )
  theta <- rmvnorm(1, mun, Ln)
  
  # generate a covariance matrix
  Sn <- S0 + ( t(Y) - c(theta) )%*% t( t(Y) - c(theta) )
  Sigma <- solve(rwish(1, nu0 + n, solve(Sn) ) )
  
  THETA <- rbind(THETA, theta)
  SIGMA <- rbind(SIGMA, c(Sigma))
}


quantile( THETA[,2] - THETA[,1], prob = c(0.025, 0.5, 0.975))
mean(THETA[,2] > THETA[,1])


