library(progressr)

options(progressr.enable = TRUE)
handlers(global = TRUE)
handlers('progress')

# Using progressr ----
slow_sqrt = function(xs) {
  p = progressor(along = xs)
  lapply(xs, function(x) {
    message("Calculating the square root of ", x)
    Sys.sleep(2)
    p(sprintf("x=%g", x))
    sqrt(x)
  })
}

y = slow_sqrt(1:4)

# Manual ----
n = 100
for (i in 1:n) {
  cat(paste0(round(i / n * 100), '% completed'))
  Sys.sleep(.05)
  if (i == n) cat(': Done')
  else cat('\014')
}
