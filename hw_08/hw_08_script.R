

# R script for Homework 08

# import some useful libraries
library("ggplot2")
library("dplyr")
library("MASS")
library("stargazer")

# define some useful functions
make_matrix <- function(dset, place)
{
  ### This function will create the design matrix for regression
  #      pre_X <- dplyr::select(dset, -eval(response_var)) %>%
  #       data.matrix()

  pre_X <- dset[,-place] # place is the variable index 
  int <- rep(1, nrow(pre_X))
  X <- cbind(int, pre_X) %>%
    data.matrix()
  return(X)
}

bayes_regress <- function(response_vec, design_matrix, nu0_val, s20_val, nreps)
{
  ### This function will run a bayesian linear regression
  g <- length(response_vec)
  nu0 <- nu0_val
  s20 <- s20_val
  S <- nreps
  n <- dim(design_matrix)[1]
  p <- dim(design_matrix)[2]
  
  
  Hg <- (g/(g+1)) * design_matrix %*% solve(t(design_matrix)%*%design_matrix) %*% t(design_matrix)
  SSRg <- t(response_vec) %*% (diag(1, nrow=n) - Hg) %*% response_vec
  
  s2 <- 1 / rgamma(S, (nu0 + n)/2, (nu0*s20+SSRg)/2 )
  
  Vb <- g * solve(t(design_matrix) %*% design_matrix) / (g + 1)
  Eb <- Vb %*% t(design_matrix) %*% response_vec
  
  E <- matrix(rnorm(S * p, 0, sqrt(s2)), S, p )
  beta <- t(  t(E%*%chol(Vb)) + c(Eb) )
  return(beta)
}

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

X <- make_matrix(dset = diabetes, c(2,8))

beta <- bayes_regress(response_vec = y
                      , design_matrix = X
                      , nu0_val = 2
                      , s20_val = 1
                      , nreps = 1000
                      )

beta_df <- beta %>%
  data.frame()

# Obtain posterior cofidence intervals for all parameters

looper_matrix <- matrix(0, nrow = dim(beta_df)[2], ncol = 3)

for(i in 1:dim(beta_df)[2])
{
  one <- mean(beta_df[,i])
  two <- mean(beta_df[,i]) - (1.96*sd(beta_df[,i]) )
  three <- mean(beta_df[,i]) + (1.96*sd(beta_df[,i]) )
  looper_matrix[i,1] <- one
  looper_matrix[i,2] <- two
  looper_matrix[i,3] <- three
}

stargazer(cbind(names(beta_df), looper_matrix)
          , summary = F
          , type="text"
          , title = "95% Posterior Confidence Intervals - Diabetes Regression"
)


# 9.2.b

# Hold off on this one for now

y <- diabetes$glu
dm <- dplyr::select(diabetes, -glu, -diabetes) %>%
  as.matrix()
z <- rep(1,ncol(dm))

out_data <- modelselect(X = dm, y = y, nreps = 1000, z = z)

# Find the probability
# isolate the z vector from the list output

z <- out_data[[2]]

outer <- rep(0,dim(z)[2])
for(i in 1:dim(z)[2])
{
outer[i] <- mean(z[,i])
}

rbind(names(data.frame(dm)), outer) %>%
  data.frame()


# now, determining posterior confidence intervals for all parameters

b <- out_data[[1]]
s <- out_data[[3]]

outer <- matrix(0, nrow = dim(b)[2], ncol = 3)
for(i in 1:dim(b)[2])
{
  outer[i,1] <- mean(b[,i])
  outer[i,2] <- quantile(b[,i], c(0.025))
  outer[i,3] <- quantile(b[,i], c(0.975))
}

dm_m <- make_matrix(diabetes, c(2,8))

output <- cbind(names(data.frame(dm_m)), outer) %>% 
  data.frame()
names(output) <- c("variable", "mean", "lower bound", "upper bound")
output

dim(out_data[[1]])



head(out_data[[1]], n = 50)

mean(out_data[[1]])

plot(density(out_data[[1]]))

# 9.3.a

# Bring in the data - should already be available within the MASS library

?UScrime # for built-in description of the data
str(UScrime)

dim(UScrime)

# Fit a regression model using the g-prior with g = n, v0 = 2, sigma20 = 1
# going to use the same code as with 9.2.a

y <- UScrime$y

X <- make_matrix(dset = UScrime, place = 16)

bayes_out <- bayes_regress(y, X, 2, 1, 1000) %>%
  data.frame()


# Obtain means and confidence intervals and put them in a nice table

filler <- matrix(0, nrow = dim(bayes_out)[2], ncol = 3)

for(i in 1:dim(bayes_out)[2]) 
  {
    zero <- mean(bayes_out[,i])
    one <- mean(bayes_out[,i]) - (1.96 * sd(bayes_out[,i]))
    two <- mean(bayes_out[,i]) + (1.96 * sd(bayes_out[,i]))
    filler[i,1] <- zero
    filler[i,2] <- one
    filler[i,3] <- two
  }

name_vec <- names(bayes_out)

filler_df <- cbind(data.frame(filler), name_vec)
names(filler_df) <- c("Bayes Mean" ,"Bayes LB", "Bayes UB", "variable")

filler_df


# Now generate the least squares estimates
LS_estimates <- (solve(t(X) %*% X)) %*% (t(X) %*% y)

compare_df <- cbind(filler_df, LS_estimates)

stargazer(compare_df
          , summary = F
          , type = "text"
          , title = "Regression Coefficients")


### 9.3.b

# Randomly divide the crime set roughly in half, into training and test sets

set.seed(1738)
sample_sorter <- runif(n = nrow(UScrime), min = 0, max = 1 )
for_sample <- cbind(UScrime,  sample_sorter)

train <- filter(for_sample, sample_sorter <= 0.5)
test <- filter(for_sample, sample_sorter > 0.5)

# the immediately below needs to be TRUE
nrow(train)
nrow(test)
(nrow(train) + nrow(test)) == nrow(UScrime)


## 9.3.b.i - Using only the training set, obtain least squares regression coefficients

X <- make_matrix(train, c(16,17))

y <- dplyr::select(train, y) %>%
  data.matrix()

train_beta <- (solve(t(X) %*% X) ) %*%(t(X) %*% y)

# Now that we have the training coefficients, use them to predict the test set

test_dm <- make_matrix(test, c(16,17))

test_y <- dplyr::select(test, y) %>%
  data.matrix()

# make the prediction

test_pred <- test_dm %*% train_beta

pre_error <- cbind(test_y, test_pred)

plot(test_y, test_pred
     , main = "Test Set - Predicted vs. Actual" 
     , xlab = "Actual"
     , ylab = "Predicted")

(ls_error <- sum((test_y - test_pred) ** 2) / nrow(test_y))


## 9.3.b.ii
# Obtain the posterior means of Beta|y using the g-prior above and only the training data

train_dat <- make_matrix(train, c(16,17))

train_y <- dplyr::select(train, y) %>%
  data.matrix()

train_bayes <- bayes_regress(train_y, train_dat, 2,1,1000) %>%
  data.matrix()

train_bayes_means <- matrix(0, nrow = ncol(train_bayes), ncol = 1)
for(i in 1:ncol(train_bayes))
  {
  train_bayes_means[i,1] <- mean(train_bayes[,i])          
  }

train_bayes_means

# now that we have the training conditional betas, let's score the test set

test_dm <- make_matrix(test, c(16,17) )

test_hat <- test_dm %*% train_bayes_means

# pull in the original y values for the test set

test_actual <- dplyr::select(test, y) %>%
  as.matrix()

plot(test_hat, test_actual
     , main = "Test Set - Bayes Predicted vs. Actual"
     , xlab = "Predicted"
     , ylab = "Actual"
     )

# compute the prediction error

(bayes_error <- sum((test_actual - test_hat) ** 2) / nrow(test_actual))


# 9.3.c. - repeat the procedure in part B many times, compute the average prediction error

# create a for-loop with 1000 repititions

nreps <- 1000
looper_mat <- matrix(0, nrow = nreps, ncol = 2)

for(j in 1:nreps)
{
set.seed(j)
sample_sorter <- runif(n = nrow(UScrime), min = 0, max = 1 )
for_sample <- cbind(UScrime,  sample_sorter)

train <- filter(for_sample, sample_sorter <= 0.6)
test <- filter(for_sample, sample_sorter > 0.6)

# the immediately below needs to be TRUE
nrow(train)
nrow(test)
(nrow(train) + nrow(test)) == nrow(UScrime)


## Using only the training set, obtain least squares regression coefficients

X <- make_matrix(train, c(16,17))

y <- dplyr::select(train, y) %>%
  data.matrix()

train_beta <- (solve(t(X) %*% X) ) %*%(t(X) %*% y)

# Now that we have the training coefficients, use them to predict the test set

test_dm <- make_matrix(test, c(16,17))

test_y <- dplyr::select(test, y) %>%
  data.matrix()

# make the prediction

test_pred <- test_dm %*% train_beta

pre_error <- cbind(test_y, test_pred)

(ls_error <- sum((test_y - test_pred) ** 2) / nrow(test_y))


## 9.3.b.ii
# Obtain the posterior means of Beta|y using the g-prior above and only the training data

train_dat <- make_matrix(train, c(16,17))

train_y <- dplyr::select(train, y) %>%
  data.matrix()

train_bayes <- bayes_regress(train_y, train_dat, 2,1,1000) %>%
  data.matrix()

train_bayes_means <- matrix(0, nrow = ncol(train_bayes), ncol = 1)
for(i in 1:ncol(train_bayes))
{
  train_bayes_means[i,1] <- mean(train_bayes[,i])          
}

train_bayes_means

# now that we have the training conditional betas, let's score the test set

test_dm <- make_matrix(test, c(16,17) )

test_hat <- test_dm %*% train_bayes_means

# pull in the original y values for the test set

test_actual <- dplyr::select(test, y) %>%
  as.matrix()

# compute the prediction error

(bayes_error <- sum((test_actual - test_hat) ** 2) / nrow(test_actual))

looper_mat[j,1] <- ls_error
looper_mat[j,2] <- bayes_error

}

mean(looper_mat[,1])
mean(looper_mat[,2])

plot(density(looper_mat[,1])
     , main = "Error Distributions"
     , lty = 1)
lines(density(looper_mat[,2]), lty = 2)
mtext("LS Error straight, Bayes Error dashed")

test_hat

dim(X)
dim(beta_means)
head(beta)






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

out_data <- modelselect(X = dm, y = y, nreps = 1000, z = 

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
