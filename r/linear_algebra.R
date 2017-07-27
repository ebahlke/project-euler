library(Matrix) # for use of "det(M)" function

# vector -> number
# returns the norm of a vector
vect_norm <- function(v) {
  sqrt(sum(v^2))
}

# vector -> vector
# normalizes a vector
normalize <- function(v) {
  v/vect_norm(v)
}

# vector -> number
# dot product of two vectors
dot_product <- function(v1, v2) {
  v1 %*% v2
}

# vector vector -> number
# computes & returns the inner angle between two vectors in the standard basis for R_n
inner_angle <- function(v1, v2) {
  cosine <- dot_product(v1,v2)/(vect_norm(v1)*vect_norm(v2))
  acos(cosine)
}

# vector number boolean number -> vector
# rotates any vector in R2 by the specified angle, either clockwise or counterclockwise,
# rounding to the specified # of decimal places
rotate_2d <- function(v, angle, ccw=TRUE, roundoff=2) {
  if (ccw)
    rotation_matrix <- matrix(c(cos(angle), sin(angle), -sin(angle), cos(angle)), 2, 2)
  else
    rotation_matrix <- matrix(c(cos(angle), -sin(angle), sin(angle), cos(angle)), 2, 2)
  
  round(rotation_matrix %*% v, roundoff)
}

# matrix -> matrix
# returns the transpose of a given matrix
transpose <- function(m) {
  entries <- as.vector(m)
  matrix(entries,ncol=nrow(m),nrow=ncol(m),TRUE)
}

# matrix natural -> matrix
# inverts the given matrix by the cofactor/minor method, rounding to the specified # of digits
mat_inverse <- function(m, roundoff=6) {
  if (nrow(m) != ncol(m))
    print("ERROR: Cannot invert a non-square matrix.")
  else {
    if (det(m) == 0)
      print("ERROR: Cannot invert a matrix with zero determinant.")
    else {
      # blank/dummy matrices:
      matrix_of_minors <- matrix(1:length(m), nrow(m), ncol(m))
      cofactor_matrix <- matrix(1:length(m), nrow(m), ncol(m))
      
      # fill the matrix of minors & cofactor matrix:
      for (i in 1:nrow(m)) {
        for (j in 1:nrow(m)) {
          reduced_matrix <- get_reduced_matrix(m, i,j)
          matrix_of_minors[i,j] <- det(reduced_matrix)
          if (((i+j) %%2) == 0)
            cofactor_matrix[i,j] <- 1
          else cofactor_matrix[i,j] <- -1
        }
      }
      
      inverse_matrix <- 1/det(m) * (transpose(matrix_of_minors) * cofactor_matrix)
      round(inverse_matrix, roundoff)
    }
  }
}

# matrix natural natural -> matrix
# selects the i-j'th "minor matrix", where i = the given row # and j = the given col #
# i.e. the matrix that results when you remove row i and column j from the original matrix
# we assume the matrix is square and the row and col arguments are contained in 1:nrow(matrix).
get_reduced_matrix <- function(mat, row, col) {
  # to get around a quirk in R where removing a row/column from a 2x2 matrix
  # results in an "integer" object rather than a 2x1 or 1x2 matrix (so you can't
  # then do the second step of matrix <- matrix[, -col] since as far as R is concerned
  # the object is now one-dimensional) I've separated out the 2x2 case to do some
  # necessary reprocessing:
  
  if (nrow(mat) == 2) {
    mat <- mat[-row,]
    mat <- matrix(mat,1,2)
    as.matrix(mat[,-col])
  }
  else {
    mat <- mat[-row,]
    mat <- mat[, -col]
    mat
  }
}

# m x m matrix -> vector
# returns the entries of the "upper diagonal" portion of a matrix as a vector
get.upper.diag.entries <- function(matrix) {
  to.return <- c()
  
  to.return <- unlist(foreach(i=1:nrow(matrix)) %do% matrix[i, i:nrow(matrix)])
  # using unlist here instead of .combine='c'
  # because it leads to a fivefold reduction in evaluation time
  
  to.return
}

# vectors ... natural(>=1) -> function
# provides a change-of-basis function from the standard basis to the given set of vectors

# The reason for being able to specify the dimension is illustrated by the following example:
# I have two matrices, each containing a set of 7049 (x,y)-coordinate pairs. I want to use
# the apply function in order to produce of 7049 change-of-basis functions. I can do this by
# converting the two matrices to a single 4x7049 matrix where the two original matrices sit
# side-by-side, and then saying apply(melded-matrix, 1(=over rows), change_to_basis, dim=2).
# Here I clearly have to manually specify the dimension, otherwise change_to_basis will think
# it's generating a basis in R4.  So you can give input a vector of length 4 (a b c d), but
# stipulate that the dimension is 2, and you'll get the change-to-basis function for the vectors
# (a b) and (c d).

change_to_basis <- function(..., dim=length(list(...))) {
  new_basis_matrix <- matrix(unlist(list(...)),
                             nrow=dim,
                             ncol=dim)
  change_of_basis_function <- function(u) { # vector -> vector
    mat_inverse(new_basis_matrix) %*% u     # changes from the standard basis to the basis
  }                                         # specified in change_to_basis for any vector
  change_of_basis_function
}

# matrix matrix natural -> matrix
# multiplies M1M2 with specified roundoff
mat_mult <- function(m1,m2,roundoff=6) {
  round(m1%*%m2, roundoff)
}