
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
  print(theta[i])
  print(input)
  
  fill_vec[i] <- input
}

plot(theta, fill_vec, type="o")
