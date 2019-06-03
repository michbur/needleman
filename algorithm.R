str1 <- c("G", "C", "A", "T", "T")
str2 <- c("G", "C", "A", "A", "T")

match <- 0
mismatch <- -1
gap <- -1

similarity_matrix <- sapply(str1, function(ith_str1)
       sapply(str2, function(ith_str2)
         ifelse(ith_str1 == ith_str2, match, mismatch)
       ))




get_next_move <- function(similarity_col, pos) {
  1L:(similarity_col)
}

get_next_move(similarity_matrix[, 2], 1)

