#homework 03 R script

# Question 4.1
# With a uniform (beta(1,1)) prior, County 1's posterior is beta(57, 43)

set.seed(1738)
county.1 <- rbeta(n = 5000, shape1 = 57, shape2 = 43)

# with a uniform prior, County 2's posterior is beta(30, 20)

set.seed(1739)
county.2 <- rbeta(n = 5000, shape1 = 30, shape2 = 20)

plot(
  density(county.1)
  , col="red"
  , lwd = 2
  )
lines(density(county.2), col="aquamarine4", lwd=2)

legend(0.35, 7.5
       , c("County 1", "County 2") 
       , lty = c(1,1)
       , lwd = c(5,5)
       , col=c("red", "aquamarine4")
       )


# what is the probability that theta.1 is less than theta.2

mean(county.1 < county.2)

# Question 4.2
# Part A

y.a <- c(12,9,12,14,13,13,15,8,15,6)
y.b <- c(11,11,10,9,9,8,7,10,6,8,8,9,7)

n.a <- length(y.a)
n.b <- length(y.b)

a.a <- 120; b.a <- 10
a.b <- 12; b.b <- 1

sum.a <- sum(y.a)
sum.b <- sum(y.b)

# Conduct the sampling - 5000 realizations for each of these
species.a <- rgamma(n = 5000, (a.a + sum.a), (b.a + n.a) )
species.b <- rgamma(n = 5000, (a.b + sum.b) , (b.b + n.b) )

plot(density(species.a), col="darkgreen", lwd = 2, xlim = c(6,15))
lines(density(species.b), col = "darkorange3", lwd = 2)

mean(species.b < species.a)

# 4.2.B
# The actual vector length of y.b is 13, so I will work exercise for 
# length(y.b) belonging to [11,15]

# will use some of the global vars created above - the posterior
# for theta.a uses the same prior, so species.a will remain constant

# let's just use a loop from 1:20

loop.n <- 50
output.vec <- rep(0,loop.n)

for(i in 1:loop.n)
{
loop.a <- 12 * i
loop.b <- i
set.seed(i*2)
species.b <- rgamma(n = 5000, (loop.a + sum.b) , (loop.b + n.b) )
output.obj <- mean(species.b < species.a)
output.vec[i] <- output.obj
}

summary(output.vec)
plot(density(output.vec))
output.vec

plot(output.vec
     , main = expression(paste("Pr(", theta, ".b < ", theta, ".a)"))
     , xlab = "n.o values"
     , ylab = expression(paste("Pr(", theta, ".b < ", theta, ".a)"))
     )
mtext("With varying theta.a prior inputs")

# 4.2.c
# Repeat the previous two questions, using posterior predictive distributions

# again using the global.vars from above, take random draws from our posterior

# repeating part A
species.a <- rgamma(n = 5000, (a.a + sum.a), (b.a + n.a) )
species.b <- rgamma(n = 5000, (a.b + sum.b) , (b.b + n.b) )

# now, use those theta values to take a random draw from a poisson distribution
# recall that poisson was the likelihood function of our data

species.a.pred <- rpois(5000, species.a)
species.b.pred <- rpois(5000, species.b)

mean(species.b.pred < species.a.pred)

# repeating part B

species.a <- rgamma(n = 5000, (a.a + sum.a), (b.a + n.a) )
species.a.pred <- rpois(5000, species.a)

loop.n <- 50
output.vec <- rep(0,loop.n)

for(i in 1:loop.n)
{
  loop.a <- 12 * i
  loop.b <- i
  set.seed(i*2)
  species.b <- rgamma(n = 5000, (loop.a + sum.b) , (loop.b + n.b) )
  species.b.pred <- rpois(5000, species.b)
  
  output.obj <- mean(species.b.pred < species.a.pred)
  output.vec[i] <- output.obj
}

summary(output.vec)

plot(output.vec
     , main = "Pr(Y.b < Y.a)"
     , xlab = "n.o values"
     , ylab = "Pr(Y.b < Y.a)"
)
mtext("With varying theta.a prior inputs")

# same kind of result as with 4.2.b - once the prior gets out of control 
# your results shift pretty dramatically



# 4.8
# Part A
# First, read in the data from the two sets
# use scan function since 

bach <- scan(
  file = "C:/Users/Philip/Schools/TAMU/STAT_638/Homework_Assignments/HW_03/menchild30bach.dat"
  )

no.bach <- scan(
  file = "C:/Users/Philip/Schools/TAMU/STAT_638/Homework_Assignments/HW_03/menchild30nobach.dat"
)


# let's take a look - poisson dist (count data) so use a barchart
barplot(table(no.bach))
barplot(table(bach))

# we are going to use a gamma(2,1) prior for each of these things


n.bach <- length(bach); n.no.bach <- length(no.bach)
a.bach <- 2; b.bach <- 1
a.no.bach <- 2; b.no.bach <- 1

sum.bach <- sum(bach); sum.no.bach <- sum(no.bach)

# calculate the posteriors

bach.post <- rgamma(n = 5000, (a.bach + sum.bach), (b.bach + n.bach))
no.bach.post <- rgamma(n = 5000, (a.no.bach + sum.no.bach), (b.no.bach + n.no.bach))

# use those in a poisson to finish our posterior predictive distribution
bach.pred <- rpois(5000, bach.post)
no.bach.pred <- rpois(5000, no.bach.post)



barplot(table(bach.pred))
barplot(table(no.bach.pred))

input <- as.matrix(rbind(table(bach.pred), table(no.bach.pred)))
input[1,9] <- 0
input

barplot(input, beside = T
        , main = "Monte Carlo Approx. for Post. Dist."
        , xlab = "Number of Children"
        , ylab = "Frequency"
        , col = c("dodgerblue3", "darkolivegreen")
        )

legend(10, 1500
       , c("Bach", "No Bach") 
       , lty = c(1,1)
#       , lwd = c(5,5)
       , c("dodgerblue3", "darkolivegreen")
)

# 4.8.B
# Find 95% quantile-based posterior confidence intervals for 
# (theta.b - theta.a) and (Y.b - Y.a) 

theta.diff <- (no.bach.post - bach.post)
plot(density(theta.diff)
     , main = expression(paste("Difference between ", theta, ".b and ", theta, ".a"))
     , xlab = expression(paste(theta, ".b - ", theta, ".a"))
     , col = "darkorchid4"
     , lwd = 2
     )
quantile(theta.diff, c(0.025, 0.975))
summary(theta.diff)

pred.diff <- (no.bach.pred - bach.pred)
barplot(table(pred.diff)
        , main = "Difference in Posterior Pred. Dists. Y.b - Y.a"
        , xlab = "Difference"
        , ylab = "Frequency"
        , col = "darkorange4"
        )
quantile(pred.diff, c(0.025, 0.975))
summary(pred.diff)

# 4.8.C
# The empirical distribution will just be the bar chart from above
barplot(table(no.bach)
        , main = "Empirical Distribution for Group B"
        , col = "darkred"
        , xlab = "Number of Children"
        , ylab = "Frequency"
        )

plot(ecdf(no.bach)
     , main = "ECDF for Group B"
     
     )

# Let's do some draws with theta.hat = 1.4

compare <- rpois(n = length(no.bach), 1.4)

input <- as.matrix(rbind(table(no.bach), table(compare)))

barplot(input, beside = T, col = c("dodgerblue3", "darkolivegreen")
        , main = "Group B Empirical vs. Theoretical Poisson"
        , xlab = "Count of Children"
        , ylab = "Frequency"
        )

legend(10, 75
       , c("Empirical", "Simulation") 
       , lty = c(1,1)
       , lwd = c(5,5)
       , c("dodgerblue3", "darkolivegreen")
)

# Not the best fit - notice the difference in relative frequency on one and two

# 4.8.d
# for each of the 5000 theta.b values (no.bach.post object), sample 
# 218 poisson random variables, and count the number of 0's and 1's

count.0.output <- rep(0, length(no.bach.post))
count.1.output <- rep(0, length(no.bach.post))

for(i in 1:length(no.bach.post))
{
input <- rpois(218, lambda = no.bach.post[i])
count.0 <- length(input[which(input==0)])
count.1 <- length(input[which(input==1)])
count.0.output[i] <- count.0
count.1.output[i] <- count.1
}

plot(count.0.output, count.1.output
     , main = "Group B - Plot of 0 by 1"
     , xlab = "Men with Zero Children"
     , ylab = "Men with One Child"
     )
# cor(count.0.output, count.1.output)

# mod <- lm(count.1.output ~ count.0.output)
# abline(mod)
# summary(mod)
table(no.bach)
abline(v = 74, col = "dodgerblue")
abline(h = 49, col = "darkred")
