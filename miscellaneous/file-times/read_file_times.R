library(dplyr)
library(readr)
library(lubridate)

# read timestamps from files and make a plot of the time differences
# `file_times` is produced by running: `l | cut -c 36-40 > file_times`
times = readr::read_csv2(file = 'file_times', col_types = 't', col_names = FALSE)
times = times$X1

data = list()
for(i in 1:(length(times)-1)) {
  data[[i]] = difftime(times[i+1], times[i], units = 'hours') %>% as.numeric
}

data = data %>% unlist()

data[32] = 0.78 # change of hour in midnight gets minus duration

plot(1:length(data), data, xlab = 'Model Number (id)', ylab = 'time to process (Hours)')

tbl_data = cbind(model_num = 1:length(data), time = data) %>% as_tibble()

linear_model = lm(time ~ model_num, data =  tbl_data)
new = data.frame(model_num = 21:150)

pred = predict(linear_model, newdata = new, se.fit = TRUE)
pred$fit %>% sum()

