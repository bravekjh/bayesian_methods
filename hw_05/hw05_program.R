
# R script for homework 5

# first, import the necessary data

bach <- scan(
  
  file = "C:/Users/Philip/Schools/TAMU/STAT_638/Homework_Assignments/HW_03/menchild30bach.dat"
  
)



no.bach <- scan(
  
  file = "C:/Users/Philip/Schools/TAMU/STAT_638/Homework_Assignments/HW_03/menchild30nobach.dat"
  
)


# let's let pop A be the bachelor pop, and B be the no bachelor pop

y <- c(bach, no.bach)

# starting values

S<- 1000
PHI <- matrix(nrow=S, ncol=2)
PHI[1,] <- phi <- c(1, 1)
diff.matrix <- matrix(nrow=S, ncol=1)
diff.matrix[1,] <- 1

set.seed(1738)
for(s in 2:S){
  
# draw a sample value of lambda

lambda <- rgamma(1,sum(y) + 8, 8 + phi[2])

# draw a sample value of theta

theta <- ( rgamma(1, sum(y) + 2, phi[1] + 1*sum(y) + 8*sum(y) ) * lambda)

phi <- c(theta, lambda)

PHI[s,] <- phi

theta.a <- theta
theta.b <- theta*lambda
diff <- theta.b - theta.a
diff.matrix[s] <- diff
}

plot(density(PHI[,1]))
plot(density(PHI[,2]))

plot(density(diff.matrix[,1]))

plot(PHI[,1])
plot(PHI[,2])
