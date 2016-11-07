

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




# additional problem

#In addition, use the missing data program under the R Code link to 
#analyze the bloodwork data (found under Data Sets).
#This program has ample comments that will help you in running it.
#Generate 10,000 observations from the posterior using this program.
#Produce the same sort of summary of the output as I did in the 
#class notes on pp. 168-172.  For each parameter, compute an 
#approximate 95% credible interval in two ways.  
#One way is to find the 2.5th and 97.5th percentiles 
#of the generated parameter values.  Another way is to use 
#mean +/- 1.96(sd).  Compare the intervals you get using the two 
#different methods.

bldwrk <- read.table("C:/Users/Philip/Schools/TAMU/STAT_638/Homework_Assignments/HW_06/bloodwork.txt")
head(bldwrk, n = 5)

missing=function(Y,theta,Sigma,nreps){
  #
  # For fun we'll save the generated values that replace the missing
  # data.  We'll put them in an nreps x 2 matrix called output.   
  #  
  output=matrix(0,nreps,2)
  Theta=matrix(0,nreps,3)
  #
  # Initialize the three-dimensional array where the values of Sigma will
  # be saved.   
  #  
  Sigma.out=array(0,c(3,3,nreps))
  n=nrow(Y)
  #
  # Specify the nonmissing data in the two rows that have missing data.   
  #
  y1=matrix(Y[42,1:2],2,1)
  y2=matrix(Y[85,c(1,3)],2,1)
  #
  # Generate two observations that will be used in place of the missing
  # values.  In doing so we use the starting parameters theta and Sigma. 
  #
  V=Sigma[1:2,1:2]
  V=solve(V)
  Cov=matrix(Sigma[3,1:2],1,2)
  v=Sigma[3,3]
  mu=theta[3]+as.vector(Cov %*% V %*% (y1-matrix(theta[1:2],2,1)))
  var1=v-as.vector(Cov %*% V %*% t(Cov))
  Y[42,3]=rnorm(1,mean=mu,sd=sqrt(var1))
  V=Sigma[c(1,3),c(1,3)]
  V=solve(V)
  Cov=matrix(Sigma[2,c(1,3)],1,2)
  v=Sigma[2,2]
  mu=theta[2]+as.vector(Cov %*% V %*% (y2-matrix(theta[c(1,3)],2,1)))
  var1=v-as.vector(Cov %*% V %*% t(Cov))
  Y[85,2]=rnorm(1,mean=mu,sd=sqrt(var1))
  theta=matrix(theta,1,3)
  #
  # Generate the nreps observations using Gibbs sampling.
  #  
  for(j in 1:nreps){
    #
    #  Compute the sample mean vector using the n x 3 "data" matrix that
    #  includes the two most recently generated pseudo observations.
    #
    ybar=matrix(1,1,n)%*%Y/n
    #
    #   Generate theta given Y, Sigma and Ymiss, where Ymiss is a 2-vector
    #   containing the current pseudo observations.  
    #    
    theta=rjmvnorm(1,as.vector(ybar),Sigma/n)
    #
    #   Compute the covariance matrix of the current n x 3 data matrix.
    #    
    S2=(n-1)*cov(Y)/n
    #
    #   Generate Sigma given Y, theta and Ymiss. 
    #    
    param=theta-ybar
    param=n*S2+n*t(param)%*%param
    param=solve(param)
    Sigma=rjWishart(1,n,param)
    Sigma=solve(Sigma[1:3,1:3,1])
    #
    #   Generate Ymiss given Y, theta and Sigma.  
    #    
    V=Sigma[1:2,1:2]
    V=solve(V)
    Cov=matrix(Sigma[3,1:2],1,2)
    v=Sigma[3,3]
    mu=theta[1,3]+as.vector(Cov %*% V %*% (y1-matrix(theta[1,1:2],2,1)))
    var1=v-as.vector(Cov %*% V %*% t(Cov))
    Y[42,3]=rnorm(1,mean=mu,sd=sqrt(var1))
    V=Sigma[c(1,3),c(1,3)]
    V=solve(V)
    Cov=matrix(Sigma[2,c(1,3)],1,2)
    v=Sigma[2,2]
    mu=theta[1,2]+as.vector(Cov %*% V %*% (y2-matrix(theta[1,c(1,3)],2,1)))
    var1=v-as.vector(Cov %*% V %*% t(Cov))
    Y[85,2]=rnorm(1,mean=mu,sd=sqrt(var1))
    output[j,]=c(Y[42,3],Y[85,2])
    Theta[j,]=theta[1,1:3]
    #
    #   Note how we assign Sigma to the jth tier of Sigma.out.    
    #
    Sigma.out[1:3,1:3,j]=Sigma
  }
  #
  # The three components of the output are put in a list.
  #  
  list(output,Theta,Sigma.out)
}

rjmvnorm=function(n,mu,Sigma){
  p=length(mu)
  X=matrix(rnorm(p*n),p,n)
  Sigroot=t(chol(Sigma))
  X=Sigroot %*% X + mu
  t(X)
}

rjWishart=function(n,nu,M){
  p=nrow(M)
  out=array(0,c(p,p,n))
  for(j in 1:n){
    Z=rjmvnorm(nu,rep(0,len=p),M)
    out[1:p,1:p,j]=t(Z)%*%Z
  }
  out
}

