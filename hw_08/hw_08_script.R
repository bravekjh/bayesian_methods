

# R script for Homework 08

library("ggplot2")
library("dplyr")

# 9.2

diabetes <- read.table("C:/Users/Philip/Schools/TAMU/STAT_638/Homework_Assignments/HW_08/azdiabetes.dat"
                       , header = T
                       )
str(diabetes)

# Let's take a look at the response variable

summary(diabetes$glu)
boxplot(diabetes$glu)
qplot(glu, data = diabetes, geom = "density", kernel = "gaussian")

# now, let's actually go through the modeling process

y <- diabetes$glu
pre_X <- select(diabetes, -glu, -diabetes) %>%
  data.matrix()

int <- rep(1, nrow(pre_X))

X <- cbind(int, pre_X)

g <- length(y)
nu0 <- 2
s20 <- 1
S <- 1000
n <- dim(X)[1]
p <- dim(X)[2]

Hg <- (g/(g+1)) * X %*% solve(t(X)%*%X) %*% t(X)
SSRg <- t(y) %*% (diag(1, nrow=n) - Hg) %*% y

s2 <- 1 / rgamma(S, (nu0 + n)/2, (nu0*s20+SSRg)/2 )

Vb <- g * solve(t(X) %*% X) / (g + 1)
Eb <- Vb %*% t(X) %*% y

E <- matrix(rnorm(S * p, 0, sqrt(s2)), S, p )
beta <- t(  t(E%*%chol(Vb)) + c(Eb) )

head(beta)

beta_df <- beta %>%
  data.frame()

# Intercept

qplot(int, data = beta_df, geom = "density"
      , main = "Intercept Density")

(beta_df$int %>%
  mean() ) - (1.96 * sd(beta_df$int))

(beta_df$int %>%
   mean() ) + (1.96 * sd(beta_df$int))

# NPREG

qplot(npreg, data = beta_df, geom = "density"
      , main = "NPREG Density")

(beta_df$npreg %>%
   mean() ) - (1.96 * sd(beta_df$npreg))

(beta_df$npreg %>%
   mean() ) + (1.96 * sd(beta_df$npreg))

# BP

qplot(bp, data = beta_df, geom = "density"
      , main = "BP Density")

(beta_df$bp %>%
   mean() ) - (1.96 * sd(beta_df$bp))

(beta_df$bp %>%
   mean() ) + (1.96 * sd(beta_df$bp))

# Skin

qplot(skin, data = beta_df, geom = "density"
      , main = "Skin Density")

(beta_df$skin %>%
   mean() ) - (1.96 * sd(beta_df$skin))

(beta_df$skin %>%
   mean() ) + (1.96 * sd(beta_df$skin))

# BMI

qplot(bmi, data = beta_df, geom = "density"
      , main = "BMI Density")

(beta_df$bmi %>%
   mean() ) - (1.96 * sd(beta_df$bmi))

(beta_df$bmi %>%
   mean() ) + (1.96 * sd(beta_df$bmi))

# PED

qplot(ped, data = beta_df, geom = "density"
      , main = "PED Density")

(beta_df$ped %>%
   mean() ) - (1.96 * sd(beta_df$ped))

(beta_df$ped %>%
   mean() ) + (1.96 * sd(beta_df$ped))

# Age

qplot(age, data = beta_df, geom = "density"
      , main = "age Density")

(beta_df$age %>%
   mean() ) - (1.96 * sd(beta_df$age))

(beta_df$age %>%
   mean() ) + (1.96 * sd(beta_df$age))


# 






# Let's define a few functions for modeling

rjmvnorm=function(n,mu,Sigma){
  #
  #  This function generates n values from a MVN(mu,Sigma)
  #  distribution. 
  #
  p=length(mu)
  X=matrix(rnorm(p*n),p,n)
  Sigroot=t(chol(Sigma))
  X=Sigroot %*% X + mu
  t(X)
}

odds=function(X,y,z1,z2){
  #
  #  This function computes the odds ratio used to calculate the
  #  posterior probability that the jth component of z is 1, given
  #  values of all the other components of z.
  #
  n=nrow(X)
  p=ncol(X)
  I=diag(rep(1,len=n))
  X=cbind(rep(1,len=n),X)
  vec1=(1:p)[z1==1]
  vec2=(1:p)[z2==1]
  p1=length(vec1)
  p2=length(vec2)
  odds=(1+n)^((p2-p1)/2)
  Xz1=cbind(rep(1,len=n),X[,1+vec1])
  Xz2=cbind(rep(1,len=n),X[,1+vec2])
  RSS1=t(y)%*%(I-(n/(n+1))*Xz1%*%solve(t(Xz1)%*%Xz1)%*%t(Xz1))%*%y
  RSS1=as.vector(RSS1)
  RSS2=t(y)%*%(I-(n/(n+1))*Xz2%*%solve(t(Xz2)%*%Xz2)%*%t(Xz2))%*%y
  RSS2=as.vector(RSS2)
  s21=lsfit(Xz1,y,intercept=F)$resid
  s21=sum(s21^2)/(n-p1-1)
  s22=lsfit(Xz2,y,intercept=F)$resid
  s22=sum(s22^2)/(n-p2-1)
  odds=odds*(s21/s22)^(1/2)
  num=s22+RSS2
  denom=s21+RSS1
  odds=odds*(num/denom)^((n+1)/2)
  odds
}

modelselect=function(X,y,nreps,z){
  #
  #  This function uses Gibbs sampling to draw samples from the posterior
  #  distribution of z given regression data, where z is a vector of
  #  0s and 1s indicating which independent variables are included in the
  #  model. X is the design matrix, y the vector of responses, nreps the
  #  desired number of samples to draw, and z a starting value for the
  #  model.  Taking z to be a vector of ncol(X) 1s is reasonable.  (This is
  #  the full model containing all the independent variables.)
  #
  #  Values of regression coefficients and error variance are also
  #  generated from their posterior each time a value of z is drawn. A
  #  g-prior (with g=n) is used for the regression coefficients and a unit
  #  information conjugate prior is used for the error variance.  All z
  #  are equally likely a priori.
  #
  p=ncol(X)
  n=nrow(X)
  I=diag(rep(1,len=n))
  # 
  #  Initialize Beta (matrix of all generated regression
  #  coefficients), Sigma2 (vector of generated error variances) and Z
  #  (matrix of all generated models). 
  #
  Beta=matrix(0,nreps,p+1)
  Sigma2=1:nreps
  Z=matrix(0,nreps,p)
  #
  #  Start generating parameters.  
  #
  for(i in 1:nreps){
    vec=sample(1:p)
    y=matrix(y,n,1)
    #
    #  Update z.
    #
    for(j in 1:p){
      z1=z
      z1[vec[j]]=1
      z2=z
      z2[vec[j]]=0
      Odds=odds(X,y,z1,z2)
      prob=Odds/(1+Odds)
      z[vec[j]]=rbinom(1,1,prob)
    }
    Z[i,]=z
    vec=(1:p)[z==1]
    p1=length(vec)
    #
    #  Compute least squares estimates for current model z. 
    #
    Xz=cbind(rep(1,len=n),X[,vec])
    out=lsfit(Xz,y,intercept=F)
    resid=out$resid
    betahat=out$coef
    A=solve(t(Xz)%*%Xz)
    SSR=t(y)%*%(I-(n/(n+1))*Xz%*% A %*%t(Xz))%*%y
    SSR=as.vector(SSR)
    s2=sum(resid^2)/(n-length(vec)-1)
    rate=(s2+SSR)/2
    #
    # Generate error variance. 
    #
    Sigma2[i]=rgamma(1,(n+1)/2,rate=rate)
    Sigma2[i]=1/Sigma2[i]
    #
    #  Compute mean vector and covariance matrix of multivariate normal
    #  from which beta is drawn.
    #
    mu=n*betahat/(n+1)
    Sigma=n*Sigma2[i]*A/(n+1)
    #
    #  Generate beta.
    #
    beta=rjmvnorm(1,mu,Sigma)
    Beta[i,1]=beta[1]
    if(length(vec)>0) Beta[i,vec+1]=beta[2:(p1+1)]
  }
  #
  #  Output Beta, Z and Sigma2.
  #
  list(Beta,Z,Sigma2)
}

### For the impending regression,
# set up the design matrix, response vector, nreps, and 

design_m <- select(diabetes, -glu) %>%
  data.matrix

head(design_m)

response_vec <- select(diabetes, glu) %>%
  as.vector

mod_select <- c(1,1,1,1,1,1,0)

dim(design_m)
dim(response_vec)
length(mod_select)

mod_out <- modelselect(X = design_m
                       , y = response_vec
                       , nreps = 1000
                       , z = mod_select
                         )
