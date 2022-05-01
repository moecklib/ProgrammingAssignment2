## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This following function is a list storing all the functions to cache the result of the next function
#This function or really list only works with the next function "cacheSolve"
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
#This function sets the inverse of the above entered matrix and stores it directly under inv
#The next time you call this function it brings up the inv that was cached previously
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

#Small test to make sure the above functions actually do work
#The inverse of the matrix is matrix(c(1, -1, -1, -2), 2, 2)
my_matrix<-makeCacheMatrix(matrix(c(2,1,1,1), 2, 2))

my_matrix$get()
my_matrix$getInverse()
cacheSolve(my_matrix)
