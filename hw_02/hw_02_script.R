
# question 3.1 b

three.one <- function(x)
{
  one <- x^57
  two <- (1-x)^43
  three <- one*two
  print(three)
}

vec <- seq(0,1,0.1)
fill_vec <- rep(0, length(vec))

for(i in 1:11) 
  {
  input <- three.one(vec[i])
  print(vec[i])
  print(input)
  
  fill_vec[i] <- input
  }

plot(vec, fill_vec, type = "o")



#3.1 part c

# compute the posterior probability of theta

constant <- (gamma(58)*gamma(44)) / gamma(102)

(theta <- seq(0,1,0.1)) # prior theta values
fill_vec <- rep(0,length(theta)) # posterior theta estimates

three.three <- function(tht)
{
  one <- tht^57
  two <- (1 - tht)^57
  three <- (one * two) / constant
  print(three)
}

for(i in 1:length(theta))
{
  input <- three.three(theta[i])
  #print(theta[i])
  #print(input)
  
  fill_vec[i] <- input
}

plot(theta, fill_vec, type="o")

# 3.1 part d

theta <- seq(0,1,length.out = 1000)
fill.vec <- rep(0,1000)
   
three.four <- function(tht)
{
  one <- tht^57
  two <- (1 - tht)^57
  three <- (one * two) / constant
  print(three)
}

for(i in 1:length(theta))
{
  input <- three.four(theta[i])
  fill.vec[i] <- input
}

plot(theta, fill.vec, type="o")


# 3.1 part e

x <- seq(0,1,length.out = 1000)
y <- dbeta(x = x, shape1 = 58, shape2 = 44)

plot(x,y)


# 3.3 part a

y.a <- c(12,9,12,14,13,13,15,8,15,6)
y.b <- c(11,11,10,9,9,8,7,10,6,8,8,9,7)

plot(density(y.a), ylim=c(0,.25))
lines(density(y.b), col="blue")

# specify the parameters for population A
a.a <- 120; a.b <- 10 # prior is y ~ gamma(120, 10)
n.a <- length(y.a)
sum.y.a <- sum(y.a)


# the mean of the gamma distribution is given by (a / b)
# which in this case will be: (sum.y.a + a.a) / (a.b + n.a)
(sum.y.a + a.a) / (a.b + n.a)

# the variance of the gamma distribution is given by (a / b^2)
# which in this case will be: (sum.y.a + a.a) / (a.b + n.a)^2
(sum.y.a + a.a) / (a.b + n.a)^2

# The 95% quantile-based estimate will be found by pulling quantiles from the 
# gamma distribution with the posterior parameters (sum.y.a + a.a) / (a.b + n.a)

qgamma(c(0.025,0.975), (sum.y.a + a.a), (a.b + n.a) )


# repeat this exercise for the independent B distribution

b.a <- 12; b.b <- 1 # prior is y ~ gamma(12, 1)
n.b <- length(y.b)
sum.y.b <- sum(y.b)


# the mean of the gamma distribution is given by (a / b)
(sum.y.b + b.a) / (b.b + n.b)

# the variance of the gamma distribution is given by (a / b^2)
(sum.y.b + b.a) / (b.b + n.b)^2

# The 95% quantile-based estimate will be found by pulling quantiles from the 
# gamma distribution with the posterior parameters (a,b)

qgamma(c(0.025,0.975), (sum.y.b + b.a), (b.b + n.b) )

# compute and plot the posterior expectations of theta.b under a prior with 
# theta.b ~ gamma(12 * n.0 , n.0), for n.0 in {0, ... , 50}

# create a function

n.0.set <- seq(1,50, by = 1)
posterior.output <- rep(0, length(n.0.set))

post.gamma <- function(n.0)
{
  b.a <- (12 * n.0)
  b.b <- n.0
  n.b <- length(y.b)
  sum.y.b <- sum(y.b)
  
  # posterior expectation = mean = (a / b) = (sum.y.b + b.a) / (b.b + n.b)
  post.gamma.output <- (sum.y.b + b.a) / (b.b + n.b)
  return(post.gamma.output)
}

for(i in 1:length(n.0.set))
{
  posterior.output[i] <- post.gamma(n.0.set[i])
}

plot(n.0.set, posterior.output, type = "l")


# 3.4 part a



