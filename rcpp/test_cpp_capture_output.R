library(Rcpp)

sourceCpp("mlr3proba/trees/hello.cpp")

hello() # prints


# Capture and silence the output
capt_hello = capture.output({
  hello()
})

# prints
a = function(x) {
  hello()
  x + 1
}
aa = a(1)
aa

# doesn't print
b = function(x) {
  capt_hello
  x + 1
}
bb = b(1)
bb
