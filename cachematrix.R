## The functions in this script allow to set (and get) a matrix to the cache.
## They calculate its inverse if it doesn't already exists and caches it.
## If the inverse matrix has already been calculated the calculation is skiped,
## and the inverse matrix is just obtained from the cache


## makeCacheMatrix creates a special "vector", which is a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(solve) inv <<- solve
      getinv <- function() inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}

# cacheSolve function calculates the inverse of the matrix created with the above function.
# However, it first checks to see if the inverse matrix has already been calculated.
# If so, it gets the inverse matrix from the cache and skips the computation.
# Otherwise, it calculates the inverse of the matrix and sets it in the cache
# via the setinv function

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv
}