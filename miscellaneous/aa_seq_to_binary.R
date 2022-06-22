#########################################
# Convert Amino Acid Sequence to binary #
#########################################

library(dplyr)
library(tibble)
library(readr)
library(stringr)
library(purrr)
library(ggplot2)
library(tidyr)

# Since 2^4 = 16 < *21* < 32 = 2^5, 5 bits are enough to represent each letter
# Make all binary numbers and map the first 21 to the Amino Acid Letters
num_bits = 5
bin_num_tbl = expand.grid(replicate(num_bits, c(0,1), simplify=FALSE)) %>% as_tibble()
bin_num_tbl = bin_num_tbl[,ncol(bin_num_tbl):1]
bin_num_vec = apply(bin_num_tbl, 1, function(row) paste0(row, collapse = ''))
unique_letters = LETTERS[1:21]
names(bin_num_vec) = unique_letters
bin_num_vec = bin_num_vec[!is.na(names(bin_num_vec))]
stopifnot(names(bin_num_vec) == unique_letters)

# generate 100 amino acid character sequences for testing
set.seed(42)
char_seqs = sapply(1:100, function(x) {
  # 3 sizes to play with
  char_len = sample(x = c(20,50,80), size = 1)
  sample(unique_letters, size = char_len, replace = TRUE) %>% paste0(collapse = '')
})

# convert to binary
seq_to_binary = function(seq) {
  seq_letters = stringr::str_split(seq, pattern = '') %>% unlist()

  seq_letters %>%
    purrr::map_chr(~ bin_num_vec[.x]) %>% # map each letter to the respective binary code
    paste0(collapse = '') # combine to one binary sequence
}

bin_seqs = sapply(char_seqs, seq_to_binary)
max_len = stringr::str_length(string = bin_seqs) %>% max()

# make all sequences the same length (the max)
padded_bin_seqs = sapply(bin_seqs, function(bin_seq) {
  seq_len = stringr::str_length(bin_seq)

  if (seq_len < max_len) {
    zeros = paste0(rep('0', max_len - seq_len), collapse = '')
    bin_seq = paste0(bin_seq, zeros)
    stopifnot(stringr::str_length(bin_seq) == max_len)
  }

  return(bin_seq)
})

# trim binary strings to specific length e.g. 42 bits (counting from left)
trimed_seqs = strtrim(padded_bin_seqs, width = 42)
