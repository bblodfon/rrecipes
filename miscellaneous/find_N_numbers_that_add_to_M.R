library(dplyr)

# Select N random numbers that add up to another (M)

set.seed(42)

# how many numbers?
n = 2
# adding to?
m = 20
# min value for random numbers?
min_value = 1

a = sapply(1:10000, function(x) {
  vec = floor(runif(n-1, min = min_value, max = m)) # take a list of random (n-1) numbers
  vec = c(vec, min_value-1, m) # add minimum value and the total sum
  numbers = diff(sort(vec)) # sort vector, take differences
  stopifnot(m == sum(numbers))
  paste(numbers, collapse = ',')
})
plot(sort(table(a)))

# simpler method: two numbers from {1,m-1}, adding up to m
a2 = sapply(1:100000, function(x) {
  n1 = floor(runif(n = 1, min = 1, max = m))
  n2 = m - n1
  stopifnot(n1+n2 == m)
  if (n1 < n2)
    paste(c(n1,n2), collapse = ',')
  else
    paste(c(n2,n1), collapse = ',')
})
plot(sort(table(a2)))

# randomly distributed numbers between 1 and m
b = sapply(1:100000, function(x) {
  floor(runif(n = 1, min = 1, max = m+1))
})
plot(table(b))
