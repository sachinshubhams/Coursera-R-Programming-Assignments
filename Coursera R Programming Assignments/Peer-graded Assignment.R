## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverse_mat <- NULL
  set <- function(y) {
    x <<- y
    inverse_mat <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inverse_mat <<- inverse
  getInverse <- function() inverse_mat
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse_mat <- x$getInverse()
  if (!is.null(inverse_mat)) {
    message("getting cached data")
    return(inverse_mat)
  }
  mat <- x$get()
  inverse_mat <- solve(mat, ...)
  x$setInverse(inverse_mat)
  inverse_mat
}

my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
my_matrix$get()

my_matrix$getInverse()
cacheSolve(my_matrix)
cacheSolve(my_matrix)

