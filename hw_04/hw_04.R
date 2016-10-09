
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
# extract vector length
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


