

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
#plot(blue[,1] , blue[,2] , xlim=c(0,50), ylim=c(0,50))
#lin.mod <- lm(Y[,2] ~ Y[,1] - 1)
#summary(lin.mod)
#abline(lin.mod)

n <- dim(blue)[1]
ybar <- apply(X = blue, MARGIN = 2, FUN = mean)
Sigma <- cov(blue)

THETA.BLUE <- SIGMA.BLUE <- NULL
set.seed(1738)
for(s in 1:10000)
{
  # generate a theta value
  Ln <- solve( solve(L0) + n*solve(Sigma)  )
  mun <- Ln%*%( solve(L0)%*%mu0 + n*solve(Sigma)%*%ybar )
  theta <- rmvnorm(1, mun, Ln)
  
  # generate a covariance matrix
  Sn <- S0 + ( t(Y) - c(theta) )%*% t( t(Y) - c(theta) )
  Sigma <- solve(rwish(1, nu0 + n, solve(Sn) ) )
  
  THETA.BLUE <- rbind(THETA.BLUE, theta)
  SIGMA.BLUE <- rbind(SIGMA.BLUE, c(Sigma))
}

plot(density(THETA.BLUE), main="KDE for Theta")
mtext("Blue Crabs")
summary(c(THETA.BLUE[,1], THETA.BLUE[,2]  ))
quantile(c(THETA.BLUE[,1], THETA.BLUE[,2]  ), c(0.025, 0.975))

plot(density(SIGMA.BLUE), main="KDE for all Sigma Covariance Matrix Values")
mtext("Blue Crabs")

summary(c(SIGMA.BLUE[,1], SIGMA.BLUE[,2], SIGMA.BLUE[,3], SIGMA.BLUE[,4]))
quantile(c(SIGMA.BLUE[,1], SIGMA.BLUE[,2], SIGMA.BLUE[,3], SIGMA.BLUE[,4]) , c(0.025, 0.975))




###Now for orange crabs


mu0 <- apply(X = orange, MARGIN = 2, FUN = mean) # sample mean
L0 <- cov(orange) # sample covariance matrix
S0 <- cov(orange)
nu0 <- 4

# get a quick look at the data
#plot(orange[,1] , orange[,2] , xlim=c(0,50), ylim=c(0,50))
#lin.mod <- lm(Y[,2] ~ Y[,1] - 1)
#summary(lin.mod)
#abline(lin.mod)

n <- dim(orange)[1]
ybar <- apply(X = orange, MARGIN = 2, FUN = mean)
Sigma <- cov(orange)

THETA.ORANGE <- SIGMA.ORANGE <- NULL
set.seed(1738)
for(s in 1:10000)
{
  # generate a theta value
  Ln <- solve( solve(L0) + n*solve(Sigma)  )
  mun <- Ln%*%( solve(L0)%*%mu0 + n*solve(Sigma)%*%ybar )
  theta <- rmvnorm(1, mun, Ln)
  
  # generate a covariance matrix
  Sn <- S0 + ( t(Y) - c(theta) )%*% t( t(Y) - c(theta) )
  Sigma <- solve(rwish(1, nu0 + n, solve(Sn) ) )
  
  THETA.ORANGE <- rbind(THETA.ORANGE, theta)
  SIGMA.ORANGE <- rbind(SIGMA.ORANGE, c(Sigma))
}

plot(density(THETA.ORANGE), main="KDE for Theta")
mtext("ORANGE Crabs")
summary(c(THETA.ORANGE[,1], THETA.ORANGE[,2]  ))
quantile(c(THETA.ORANGE[,1], THETA.ORANGE[,2]  ), c(0.025, 0.975))

plot(density(SIGMA.ORANGE), main="KDE for all Sigma Covariance Matrix Values")
mtext("ORANGE Crabs")

summary(c(SIGMA.ORANGE[,1], SIGMA.ORANGE[,2], SIGMA.ORANGE[,3], SIGMA.ORANGE[,4]))
quantile(c(SIGMA.ORANGE[,1], SIGMA.ORANGE[,2], SIGMA.ORANGE[,3], SIGMA.ORANGE[,4]) , c(0.025, 0.975))


###7.3.b)

plot(THETA.BLUE[,1], THETA.BLUE[,2]
     , main="Y1 (Body Depth) vs. Y2 (Rear Width)"
     , xlab="Y1"
     , ylab="Y2"
)
mtext("Blue Crabs")


plot(density(THETA.BLUE[,1] ), lwd = 2 
     , main="KDEs for Theta.1 and Theta.2")
lines(density(THETA.BLUE[,2] ), lwd = 2, lty = 2)
mtext("Blue Crabs")
legend(5, .2, c("Theta.1", "Theta.2"), lwd =c(2,2), lty=c(1,2) )

### Repeat for Orange Crabs

plot(THETA.ORANGE[,1], THETA.ORANGE[,2]
     , main="Y1 (Body Depth) vs. Y2 (Rear Width)"
     , xlab="Y1"
     , ylab="Y2"
)
mtext("ORANGE Crabs")
summary(THETA.ORANGE[,1])

plot(density(THETA.ORANGE[,1] ), lwd = 2 
     , main="KDEs for Theta.1 and Theta.2")
lines(density(THETA.ORANGE[,2] ), lwd = 2, lty = 2)
mtext("ORANGE Crabs")
legend(5.5, .2, c("Theta.1", "Theta.2"), lwd =c(2,2), lty=c(1,2) )


### 7.3.c
# we are going to need to extract the correlation coefficient from each of the 
# covariance matrices.  

#blue crabs first
COEF.BLUE <- NULL

for(j in 1:nrow(SIGMA.BLUE))
  {
  corr.coef <- SIGMA.BLUE[j, 2] / sqrt( SIGMA.BLUE[j, 1] * SIGMA.BLUE[j, 4]  )
  COEF.BLUE <- rbind(COEF.BLUE, corr.coef)
  }



#orange crabs first
COEF.ORANGE <- NULL

for(j in 1:nrow(SIGMA.ORANGE))
{
  corr.coef <- SIGMA.ORANGE[j, 2] / sqrt( SIGMA.ORANGE[j, 1] * SIGMA.ORANGE[j, 4]  )
  COEF.ORANGE <- rbind(COEF.ORANGE, corr.coef)
}

plot(density(COEF.BLUE)
     , main="Posterior Densities for rho.blue and rho.orange"
     , lwd = 2
     )
lines(density(COEF.ORANGE), lty=2, lwd = 2)
legend(0.9, 30, c("Blue", "Orange"), lty=c(1,2), lwd=c(2,2))

mean(COEF.BLUE < COEF.ORANGE)
