
# R script for homework 5

# first, import the necessary data

bach <- scan(
  
  file = "C:/Users/Philip/Schools/TAMU/STAT_638/Homework_Assignments/HW_03/menchild30bach.dat"
  
)



no.bach <- scan(
  
  file = "C:/Users/Philip/Schools/TAMU/STAT_638/Homework_Assignments/HW_03/menchild30nobach.dat"
  
)

### ID the starting parameters
## Likelihood parameters first
y <- bach
y.bar <- mean(bach)
n <- length(bach)

z <- no.bach
z.bar <- mean(no.bach)
m <- length(z)

## now the mcmc parameters

S <- 5000 # number of Gibbs Samples
PHI <- matrix(nrow=S, ncol = 2)
PHI[1,] <- c(1,1) # gotta start somewhere
diff.matrix <- matrix(nrow=S, ncol = 1) #matrix for the differences
diff.matrix[1,] <- 1 # gotta start somewhere

theta.a <- 2; theta.b <- 1
lambda.a <- lambda.b <- 8

## now, lets enter the loop

lambda.sequence <- c(8, 16, 32, 64, 128)

lambda.a <- lambda.b <- 128
  set.seed(1840)
  for(s in 2:S)
    {
        
    # first take a draw for the theta value
    theta <- rgamma(1, (n*y.bar)+(m*z.bar)+theta.a, n + (m*PHI[s-1,2]) + theta.b  )
    
    # now take a draw for the lambda value
    lambda <- rgamma(1, (m*z.bar) - lambda.a, (m*PHI[s-1,1]) + lambda.b)
    
    phi <- c(theta, lambda)
    PHI[s,] <- phi
    
    diff.element <- lambda - theta
    diff.matrix[s] <- diff.element
    }
  
  plot(density(PHI[,1]), col = "red")
  lines(density(PHI[,2]), col = "blue")
  plot(density(diff.matrix))

pdf("C:/Users/Philip/Schools/TAMU/STAT_638/Homework_Assignments/HW_05/hw_output/lambda128.pdf")
plot(
  density(diff.matrix)
    , col = "black"
    , main = "Density of theta.b - theta.a"
  )
mtext(paste("Labmda =", 128, "  Mean =", round(mean(diff.matrix), digits = 3)  ))
dev.off()

