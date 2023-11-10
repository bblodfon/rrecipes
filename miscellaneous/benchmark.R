library(rbenchmark)

within(rbenchmark::benchmark(
 'test1' = { Sys.sleep(0.5) },
 'test2' = { Sys.sleep(0.3) },
  replications = 5,
  columns = c("test", "replications", "elapsed", "relative")
), { average_secs = (elapsed/replications)})
