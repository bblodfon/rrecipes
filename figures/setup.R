library(UpSetR)

data_list = list(
  a = c(1:10),
  b = c(5:12),
  c = c(1,7,12,14,15,16,17,18,19,20)
)

UpSetR::upset(fromList(data_list), order.by = 'freq')
