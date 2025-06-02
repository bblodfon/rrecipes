#1 Summarize all info you need in one tibble

Every data can be 2D-ified like that, since part of it will go to some 
function for analysis (matrix, tibble, data.frame) or for ploting (ggplot,...)

#2 For loops, making tibble of results

library(tidyverse)

index = 1
data_list = list()
future::plan("multisession")
res = future.apply::future_lapply(1:n_rows, function(i) {
  # do something with i
  tibble(a = a, b = a, ert = list(a_tibble_or_some_else_object)
} |> bind_rows()

res ## nice looking tibble for further visualization, preprocessing

#3 Writing parallel code for efficiency 

Same as above example but parallelized with future_lapply + progressr

#4 defensive programming using checkmate()

- checks

#5 renv for reproducibility

#6 using rbenchmark/microbenchmark/bench::mark for quick testing of which code is fastest

#7 use reprex::reprex() and github issues when collaborating on code and communicating issues!

#8 test code/functions with testthat() expect() etc
- small examples
- edge cases

#9 quarto

For scientific reports, it is truly the best there is, more easy to use

#10 using map-like functions

mlr3misc::map_*

#11 matrices subsetting

drop = FALSE issue => `matrix` becomes `numeric`, careful!

#12 use the IDE to your adventage

- code snippets

