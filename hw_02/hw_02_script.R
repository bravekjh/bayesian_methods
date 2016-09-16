
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


(gamma(58)*gamma(44)) / gamma(102)

#3.1 part c


