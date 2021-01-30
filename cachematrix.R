# A pair of functions that cache the inverse of a matrix. 
#Programming assignment for professor Peng's R programming course.


# This first function creates a special vector, whose purpose is to create and 
# store a set of functions that will be used to cache/uncache the inverted
# matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set_inv_mat <- function(inv_mat) m <<- inv_mat
  get_inv_mat <- function() m
  list(set = set, get = get,
       set_inv_mat = set_inv_mat,
       get_inv_mat = get_inv_mat)
}


# This function checks the m variable to see if the inverted matrix has been
# computed already. If so return it and exit the function otherwise proceeds
# to the computation

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$get_inv_mat()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  message("computing inverted matrix")
  m <- solve(data, ...)
  x$set_inv_mat(m)
  m
}
