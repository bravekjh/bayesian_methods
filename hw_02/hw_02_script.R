
# question 3.1 b

three.one <- function(x)
{
  one <- x^57
  two <- (1-x)^43
  three <- one*two
  return(three)
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

plot(vec, fill_vec, type = "o"
    , main = expression(paste("3.1.b Plot - Density as function of ", theta))
    , xlab = expression(theta)
    , ylab = expression(paste("Density(", theta, ")"))  
    )



#3.1 part c

# compute the posterior probability of theta

constant <- (gamma(58)*gamma(44)) / gamma(102)

(theta <- seq(0,1,0.1)) # prior theta values
fill_vec <- rep(0,length(theta)) # posterior theta estimates

three.three <- function(tht)
{
  one <- tht^57
  two <- (1 - tht)^43
  three <- (one * two) / constant
  return(three)
}

for(i in 1:length(theta))
{
  input <- three.three(theta[i])
  #print(theta[i])
  #print(input)
  
  fill_vec[i] <- input
}

fill_vec
plot(theta, fill_vec, type="o"
     , main = expression(paste("3.1.c Plot - Density as function of ", theta))
     , ylab = expression(paste("p(", theta, "|", y, ")"))
     , xlab = expression(theta)       
     )

# 3.1 part d

theta <- seq(0,1,length.out = 1000)
fill.vec <- rep(0,1000)
constant <- (gamma(58)*gamma(44)) / gamma(102)

three.four <- function(tht)
{
  one <- tht^57
  two <- (1 - tht)^43
  three <- (one * two) / constant
  return(three)
}

for(i in 1:length(theta))
{
  input <- three.four(theta[i])
  fill.vec[i] <- input
}

plot(theta, fill.vec, type="l"
     , main = expression(paste("3.1.d Plot - Density as function of ", theta))
     , ylab = expression(paste("p(", theta, "|", y, ")"))
     , xlab = expression(theta)            
     )
mtext("Using Uniform Prior")

# 3.1 part e

x <- seq(0,1,length.out = 1000)
y <- dbeta(x = x, shape1 = 58, shape2 = 44)

plot(x,y, type = "l"
     , main = expression(paste("3.1.e Plot - Density as function of ", theta))
     , ylab = expression(paste("p(", theta, "|", y, ")"))
     , xlab = expression(theta)                 
     )
mtext("Using Beta Distribution for Posterior")

which(y==max(y))
x[570]

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


# 3.3.b

# compute and plot the posterior expectations of theta.b under a prior with 
# theta.b ~ gamma(12 * n.0 , n.0), for n.0 in {0, ... , 50}

# create a function

n.0.set <- seq(1,500, by = 1)
posterior.output <- rep(0, length(n.0.set))

post.gamma <- function(n.0)
{
  b.a <- (12 * n.0)
  b.b <- n.0
  n.b <- length(y.b) # from the given data
  sum.y.b <- sum(y.b) # also from the given data
  
  # posterior expectation = mean = (a / b) = (sum.y.b + b.a) / (b.b + n.b)
  post.gamma.output <- (sum.y.b + b.a) / (b.b + n.b)
  return(post.gamma.output)
}

for(i in 1:length(n.0.set))
{
  posterior.output[i] <- post.gamma(n.0.set[i])
}

plot(n.0.set, posterior.output, type = "l"
    , main = expression(paste("Graphic for 3.3.b - posterior expectation of ", theta))   
    , xlab = expression(n)
    , ylab = expression(paste("p(", theta, "|", y, ")"))
     )
t 
max(posterior.output)
# 3.4 part a

# try out a bunch of theta values from 0 to 1
theta <- seq(0,1,length.out = 1000)

# p(theta) ~ beta(2,8) prior
a <- 2 ; b <- 8
y <- 15; n <- 43

p.theta <- dbeta(x = theta, a, b)

plot(theta, p.theta, type = "l", col = "blue"
     , main = expression(paste("Plot of p(", theta, ")"))
     , xlab = expression(theta)
     , ylab = expression(paste("p(", theta, ")"))
     ) # looks p.good

# p(y|theta) ~ (theta^15) * (1 - theta)^(43-15)
p.y.theta <- (theta^y) * (1 - theta)^(n - y)

plot(theta, p.y.theta, type="l", col = "red"
     , main = expression(paste("Plot of p(y|", theta, ")"))
     , xlab = expression(theta)
     , ylab = expression(paste("p(y|", theta, ")"))     
     )

# posterior distribution
p.theta.y <- dbeta(theta, a + y, b + n - y)

plot(theta, p.theta.y, type="l"
     , main = expression(paste("Plot of ", theta, "|y"))
     , xlab = expression(theta)
     , ylab = expression(paste("p(", theta, "|y)"))          
     )

# find the posterior mean, mode, sd

# beta mean is (a / a + b)
(a + y) / (a + y + b + n - y)

# beta mode is (a - 1) / [ (a - 1) + (b - 1)]
(a + y - 1) / ( ( a + y - 1) + (b + n - y - 1) )

# beta variance is (ab) / ( (a + b + 1)(a + b)^2 )
beta.var <- ((a + y)*(b + n - y)) / ((a + y + b + n - y + 1)*(a + y + b + n - y)^2)
sqrt(beta.var)

qbeta(c(0.025, 0.975), a + y, b + n - y)


# 3.4.b - repeat part a but for a beta(8, 2) prior

theta <- seq(0,1,length.out = 1000)

# p(theta) ~ beta(2,8) prior
a <- 8 ; b <- 2
y <- 15; n <- 43

p.theta <- dbeta(x = theta, a, b)

plot(theta, p.theta, type = "l", col = "blue"
     , main = expression(paste("Plot of p(", theta, ")"))
     , xlab = expression(theta)
     , ylab = expression(paste("p(", theta, ")"))
     ) # looks p.good

# p(y|theta) ~ (theta^15) * (1 - theta)^(43-15)
p.y.theta <- choose(n, y) * (theta^y) * (1 - theta)^(n - y)

plot(theta, p.y.theta, type="l", col = "red"
     , main = expression(paste("Plot of p(y|", theta, ")"))
     , xlab = expression(theta)
     , ylab = expression(paste("p(y|", theta, ")"))     
     )

# posterior distribution
p.theta.y <- dbeta(theta, a + y, b + n - y)

plot(theta, p.theta.y, type="l"
     , main = expression(paste("Plot of ", theta, "|y"))
     , xlab = expression(theta)
     , ylab = expression(paste("p(", theta, "|y)"))           
     )

# find the posterior mean, mode, sd

# beta mean is (a / a + b)
(a + y) / (a + y + b + n - y)

# beta mode is (a - 1) / [ (a - 1) + (b - 1)]
(a + y - 1) / ( ( a + y - 1) + (b + n - y - 1) )

# beta variance is (ab) / ( (a + b + 1)(a + b)^2 )
beta.var <- ((a + y)*(b + n - y)) / ((a + y + b + n - y + 1)*(a + y + b + n - y)^2)
sqrt(beta.var)

qbeta(c(0.025, 0.975), a + y, b + n - y)


# 3.4.c

# plot the mixture distribution with the provided density function

theta <- seq(0,1,length.out = 1000)

  part.1 <- (1 / 4)
  part.2 <- (gamma(10) / (gamma(2) * gamma(8))) 
  part.3 <- ( ((3 * theta) * (1 - theta)^7) + ((theta^7) * (1  - theta)  )   )
prior.theta <- part.1 * part.2 * part.3

plot(theta, prior.theta, type="l", ylim=c(0,4)
     , main = expression(paste("Mixture Distribution"))
     , xlab = expression(theta)
     , ylab = expression(paste("p(", theta, "|y)"))                
     )

p.theta.1 <- dbeta(theta, 2, 8)
p.theta.2 <- dbeta(theta, 8, 2)

lines(theta, p.theta.1, col = "blue")
lines(theta, p.theta.2, col = "red")
mtext("Blue = beta(2,8); Red = beta(8,2")

# 3.4.d.iii - plot the posterior for a variety of theta values

theta <- seq(0,1,length.out = 1000)

# first get the prior specified
prior.1 <- (1 / 4)
prior.2 <- (gamma(10) / (gamma(2) * gamma(8))) 
prior.3 <- ( ((3 * theta) * (1 - theta)^7) + ((theta^7) * (1  - theta)  )   )

prior.theta <- prior.1 * prior.2 * prior.3

# now specify the likelihood function

like.theta <- (theta^15)* ((1 - theta)^28)

posterior <- prior.theta * like.theta

plot(theta, posterior, type = "l"
     , main = expression(paste("posterior distribution"))
     , xlab = expression(theta)
     , ylab = expression(paste("p(", theta, "|y)"))      
     )
mtext(expression(paste(p(theta), "*", "p(y|",theta, ")")  ))

# find the approximate posterior mode - should be the top of the density curve
element <- which(posterior == max(posterior))
theta[element]

# 3.7 part a
# plotting the posterior density of theta
 
theta <- seq(0,1, length.out = 1000)

posterior <- dbeta(theta, 3, 14)

plot(theta, posterior, type="l"
     , main = expression(paste("3.7.a - posterior distribution"))
     , xlab = expression(theta)
     , ylab = expression(paste("p(", theta, "|y)"))      
          )

# 3.7.c plot the function obtained in 3.7.c

pot.values <- seq(0,289, by = 1)
output.vector <- rep(0, length(pot.values))
# the gamma function was out of range for this, so I am changing to log scale


pred.posterior <- function(y2)
{
  part.1 <- (lgamma(17) / (lgamma(3)*lgamma(14)))
  part.2 <- (lgamma(1 + y2)*lgamma(12 + 278 - y2)) / lgamma(13 + 278)
  part.3 <- part.1 * part.2
  return(part.3)
}

for(i in 1:length(pot.values))
{
  input <- pred.posterior(pot.values[i])
  output.vector[i] <- input
}

plot(pot.values, output.vector, type = "l"
     , main = "Plot for 3.7.c"
     )

# Find the mean and sd of the function
element <- which(output.vector == max(output.vector))
pot.values[element]
var(pot.values)

# 3.7.d

plot(pot.values, output.vector, type = "l"
     , main = "Plot for 3.7.d"
)

