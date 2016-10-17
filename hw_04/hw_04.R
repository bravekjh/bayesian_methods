
# The following is the code for homework 4

#  5.1.a
# read in the data

school.1 <- read.table(
              "C:/Users/Philip/Schools/TAMU/STAT_638/Homework_Assignments/HW_04/school1.dat"
              , header = F
              )

school.2 <- read.table(
              "C:/Users/Philip/Schools/TAMU/STAT_638/Homework_Assignments/HW_04/school2.dat"
              , header = F
              )

school.3 <- read.table(
              "C:/Users/Philip/Schools/TAMU/STAT_638/Homework_Assignments/HW_04/school3.dat"
              , header = F
              )

names(school.1) <- "time"
names(school.2) <- "time"
names(school.3) <- "time"

plot(density(school.1$time), main = "Various Densities")
lines(density(school.2$time), col = "dodgerblue")
lines(density(school.3$time), col = "orange")

# Now, we need to incorporate the prior info in estimating posterior densities
# same prior info will be used for all schools
mu0<- 5; s20 <- 4
k0 <- 1; nu0 <- 2

# extract vectors from the data frames
y.1 <- school.1$time; y.2 <- school.2$time; y.3 <- school.3$time
# extract vector length (number of obs)
n.1 <- length(y.1); n.2 <- length(y.2); n.3 <- length(y.3)
# extract sample means
ybar.1 <- mean(y.1); ybar.2 <- mean(y.2); ybar.3 <- mean(y.3)
# extract sample variance
s2.1 <- var(y.1); s2.2 <- var(y.2); s2.3 <- var(y.3)

# conduct posterior inference
# calculate k_sub_n
kn.1 <- k0 + n.1; kn.2 <- k0 + n.2; kn.3 <- k0 + n.3 
# calculate nu_sub_n
nun.1 <- nu0 + n.1; nun.2 <- nu0 + n.2; nun.3 <- nu0 + n.3
# posterior mean
mun.1 <- ( (k0 * mu0) + (n.1 * ybar.1) ) / kn.1
mun.2 <- ( (k0 * mu0) + (n.2 * ybar.2) ) / kn.2
mun.3 <- ( (k0 * mu0) + (n.3 * ybar.3) ) / kn.3
# posterior variance  
s2n.1 <- ( (nu0 * s20) + ( (n.1 - 1) * s2.1 ) + ( (k0 / kn.1) * n.1 * (ybar.1 - mu0)^2 )  ) / nun.1
s2n.2 <- ( (nu0 * s20) + ( (n.2 - 1) * s2.2 ) + ( (k0 / kn.2) * n.2 * (ybar.2 - mu0)^2 )  ) / nun.2
s2n.3 <- ( (nu0 * s20) + ( (n.3 - 1) * s2.3 ) + ( (k0 / kn.3) * n.3 * (ybar.3 - mu0)^2 )  ) / nun.3

# School 1
mun.1; s2n.1; sqrt(s2n.1)
# School 2
mun.2; s2n.2; sqrt(s2n.2)
# School 3
mun.3; s2n.3; sqrt(s2n.3)

# Now that the baseline info has been established, take Monte Carlo draws
# to approximate 95% Confidence Intervals for the parameters of interest

s2.sample.1 <- 1 / rgamma(n = 10000, nun.1 / 2, s2n.1 * nun.1 / 2)
s2.sample.2 <- 1 / rgamma(n = 10000, nun.2 / 2, s2n.2 * nun.2 / 2)
s2.sample.3 <- 1 / rgamma(n = 10000, nun.3 / 2, s2n.3 * nun.3 / 2)

theta.sample.1 <- rnorm(n = 10000, mun.1, sqrt(s2.sample.1 / kn.1))
theta.sample.2 <- rnorm(n = 10000, mun.2, sqrt(s2.sample.2 / kn.2))
theta.sample.3 <- rnorm(n = 10000, mun.3, sqrt(s2.sample.3 / kn.3))

# Find the posterior mean + sd, and 95% CIs for both
# School 1
mun.1; quantile(theta.sample.1, c(0.025, 0.975))
sqrt(s2n.1); quantile(sqrt(s2.sample.1), c(0.025, 0.975))
# School 2
mun.2; quantile(theta.sample.2, c(0.025, 0.975))
sqrt(s2n.2); quantile(sqrt(s2.sample.2), c(0.025, 0.975))
# School 3
mun.3; quantile(theta.sample.3, c(0.025, 0.975))
sqrt(s2n.3); quantile(sqrt(s2.sample.3), c(0.025, 0.975))

# 5.1.b

df <- data.frame(cbind(theta.sample.1, theta.sample.2, theta.sample.3
                       , s2.sample.1, s2.sample.2, s2.sample.3))
head(df)
dim(df)

plot(density(df$theta.sample.1), main = "Theta Simulation Densities")
lines(density(df$theta.sample.2), col="dodgerblue")
lines(density(df$theta.sample.3), col="orange")
legend(x = 12, y = 0.4
       , c("one","two","three")
       , lty = c(1,1)
       , col=c("black", "dodgerblue", "orange")
       )

# Permutation 1; n = 1; i = 1; j = 2; k = 3

mean( (df$theta.sample.1 < df$theta.sample.2) ) * mean(df$theta.sample.2 < df$theta.sample.3)

# Permutation 2; n =2; i = 1; j = 3; k = 2

mean( (df$theta.sample.1 < df$theta.sample.3) ) * mean(df$theta.sample.3 < df$theta.sample.2)

# Permutation 3; n=3;i = 2; j = 1; k = 3

mean( (df$theta.sample.2 < df$theta.sample.1) ) * mean(df$theta.sample.1 < df$theta.sample.3)


# Permutation 4; n = 4;i = 2; j = 3; k = 1

mean( (df$theta.sample.2 < df$theta.sample.3) ) * mean(df$theta.sample.3 < df$theta.sample.1)


# Permutation 5; i = 3, j = 1, k = 2

mean( (df$theta.sample.3 < df$theta.sample.1) ) * mean(df$theta.sample.1 < df$theta.sample.2)

# Permutation 6; i = 3, j = 2, k = 1

mean( (df$theta.sample.3 < df$theta.sample.2) ) * mean(df$theta.sample.2 < df$theta.sample.1)

# 5.1.c
# generate predictive draws from each of the schools distributions

posterior.1 <- rnorm(n = 10000, mean = theta.sample.1, sd = sqrt(s2.sample.1))
posterior.2 <- rnorm(n = 10000, mean = theta.sample.2, sd = sqrt(s2.sample.2))
posterior.3 <- rnorm(n = 10000, mean = theta.sample.3, sd = sqrt(s2.sample.3))

plot(density(posterior.1))
lines(density(posterior.2), col = "dodgerblue")
lines(density(posterior.3), col = "orange")

# Permutation 1; n = 1; i = 1; j = 2; k = 3

mean( (posterior.1 < posterior.2) ) * mean(posterior.2 < posterior.3)

# Permutation 2; n =2; i = 1; j = 3; k = 2

mean( (posterior.1 < posterior.3) ) * mean(posterior.3 < posterior.2)

# Permutation 3; n=3;i = 2; j = 1; k = 3

mean( (posterior.2 < posterior.1) ) * mean(posterior.1 < posterior.3)


# Permutation 4; n = 4;i = 2; j = 3; k = 1

mean( (posterior.2 < posterior.3) ) * mean(posterior.3 < posterior.1)


# Permutation 5; i = 3, j = 1, k = 2

mean( (posterior.3 < posterior.1) ) * mean(posterior.1 < posterior.2)

# Permutation 6; i = 3, j = 2, k = 1

mean( (posterior.3 < posterior.2) ) * mean(posterior.2 < posterior.1)

# 5.1.d
# Compute the posterior prob. that theta.1 is larger than both 
# theta.2 and theta.3

mean(df$theta.sample.1 > df$theta.sample.2) * mean(df$theta.sample.1 > df$theta.sample.3)
mean(posterior.1 > posterior.2) * mean(posterior.1 > posterior.3)

# 5.2

n.a <- n.b <- 16

ybar.a <- 75.2; s.a <- 7.3
ybar.b <- 77.5; s.b <- 8.1

# same prior for both populations
mu0 <- 75; s20 <- 100

priors <- data.frame(cbind( c(1,2,4,8,16,32), c(1,2,4,8,16,32) ))
names(priors) <- c("k0", "nu0")


# loop starts here
inputter <- rep(0,6)
ns <- c(1,2,4,8,16,32)
par(mfrow=c(2,3))
for(i in seq_along(ns)) 
{

kn <- n.a + priors$k0[i]
nun <- n.a + priors$nu0[i]

mun.a.i <- ( (priors$k0[i] * mu0) + (n.a * ybar.a) ) / kn
s2n.a.i <- ( (priors$nu0[i] * s20) + ( (n.a - 1) * s.a ) + ( (priors$k0[i] / kn) * n.a * (ybar.a - mu0)^2 )  ) / nun

mun.b.i <- ( (priors$k0[i] * mu0) + (n.b * ybar.b) ) / kn
s2n.b.i <- ( (priors$nu0[i] * s20) + ( (n.b - 1) * s.b ) + ( (priors$k0[i] / kn) * n.b * (ybar.b - mu0)^2 )  ) / nun


# posterior samples

# pop A first
s2.a.postsample <- 1 / rgamma(10000, nun / 2, s2n.a.i * nun / 2)
theta.a.postsample <- rnorm(10000, mun.a.i, sqrt(s2.a.postsample / kn))
# pop B second
s2.b.postsample <- 1 / rgamma(10000, nun / 2, s2n.b.i * nun / 2)
theta.b.postsample <- rnorm(10000, mun.b.i, sqrt(s2.b.postsample / kn))

# probabilities

plot(density(theta.a.postsample), main = paste("Graphic", i))
lines(density(theta.b.postsample), col = "dodgerblue")

inputter[i] <- mean(theta.a.postsample < theta.b.postsample)

}

inputter

par(mfrow=c(1,1))

plot(ns, inputter
     , type = "b"
     , lwd = 2
     , col = "dodgerblue"
     , main = expression(paste(nu, "0 vs. Pr(", theta, ".a < ", theta, ".b)"))
     , xlab = expression(paste(nu))
     , ylab = expression(paste("Pr(", theta, ".a < ", theta, ".b)"))
     )


