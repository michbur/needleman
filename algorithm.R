str1 <- c("G", "A", "A", "T", "G")
str2 <- c("G", "C", "A", "A", "T", "G")

match <- 0
mismatch <- -1
gap <- -1

similarity_matrix <- sapply(str1, function(ith_str1)
  sapply(str2, function(ith_str2)
    ifelse(ith_str1 == ith_str2, match, mismatch)
  ))


# pos_matrix[lower.tri(pos_matrix, diag = FALSE)] <- unlist(lapply(1L:ncol(similarity_matrix), function(i) i:ncol(similarity_matrix) - i + 1))
# pos_matrix[upper.tri(pos_matrix, diag = FALSE)] <- unlist(lapply(ncol(similarity_matrix):2L, function(i) rev(i:ncol(similarity_matrix) - i + 1)))
# pos_matrix <- pos_matrix * gap
# point_matrix <- pos_matrix + similarity_matrix


get_next_move <- function(similarity_matrix, pos, id, path = c()) {
  if(id == ncol(similarity_matrix) + 1) {
    return(path)
  } else {
    penalties <- similarity_matrix[, id] + abs(1L:length(similarity_matrix[, id]) - pos - 1) * gap
    lapply(unname(which(penalties == max(penalties))), function(i) {
      get_next_move(similarity_matrix, i, id + 1, path = c(path, i))
    })
  }
}

# nrow - number of paths
all_paths <- matrix(unlist(get_next_move(similarity_matrix, 0, 1)), ncol = ncol(similarity_matrix), byrow = TRUE)

similarity_matrix
lapply(1L:ncol(similarity_matrix), function(i) all_paths[1, i])
path_scores <- lapply(1L:nrow(all_paths), function(ith_path) 
  sum(sapply(1L:ncol(similarity_matrix), function(i) 
    similarity_matrix[all_paths[ith_path, i], i])))
