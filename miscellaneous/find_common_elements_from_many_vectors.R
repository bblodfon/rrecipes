# Find common elements across many vectors
vector_list = list(a = c(1,2,3,4,5), b = c(2,4,6,7), cc = c(9,2,9,4,3,5))
Reduce(intersect,vector_list)

