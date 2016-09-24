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


species.a <- rgamma(n = 5000, (a.a + sum.a), (b.a + n.a) )
species.b <- rgamma(n = 5000, (a.b + sum.b) , (b.b + n.b) )

plot(density(species.a), col="darkgreen", lwd = 2, xlim = c(6,15))
lines(density(species.b), col = "darkorange3", lwd = 2)

mean(species.b < species.a)



