## These set of functions create a special 'matrix' object that can cache its inverse(using makeCacheMatrix).
## The functions stored in the special matrix can then be called using cacheSolve which retrieves the inverse 
##from the cache, if the inverse of the matrix has been computed and if the matrix has not changed.
## However if the inverse has not been previously computed or the imput matrix has changed, cachSolve computes
## the inverse of the new imput and sets the inverse in the cache using the setinverse function.


# makeCacheMatrix creates a special matrix object that can cache the imput matrix and its inverse. 
#It creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) minv <<- solve
  getmatrix <- function() minv
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}



# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.
# The function assumes that the matrix is always invertible.

cacheSolve <- function(x=matrix(),...) {
  minv <- x$getmatrix()
  if(!is.null(minv)) {
    message("getting cached data")
    return(minv)
  }
  matrix <- x$get()
  minv <- solve(matrix, ...)
  x$setmatrix(minv)
  minv
}
