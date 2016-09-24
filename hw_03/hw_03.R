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

