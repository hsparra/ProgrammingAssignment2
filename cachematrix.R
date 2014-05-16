#  Set of functions to compute the inverse of a matrix and cache the
#  results for future calls.


#  This function creates a special "matrix' which is really a list
#  with a function to:
#    1. Set value of the matrix
#    2. Get value of the matrix
#    3. Set the value of the inverse matrix
#    4. Get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
   inverse <- NULL
   # When setting a new matrix reset the inverse to NULL.
   # This ensures cacheSolve does not return a stale cache.
   set <- function(y) {
      x <<- y
      inverse <<- NULL
   }
   get <- function() x
   setinverse <- function(inversematrix) inverse <<- inversematrix
   getinverse <- function() inverse
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


#  This function calculates the inverse of the special "matrix" created
#  with the makeCacheMatrix function. However, it first checks to see
#  if the inverse and if so 'get's the inverse from the cache and
#  skips the computation. Otherwise, it computes the inverse of the
#  matrix and sets the value of the inverse in the cache via the
#  'setinverse' function.

cacheSolve <- function(x, ...) {
    m <- x$getinverse
    # if already exists the retrun cached version
    if (!is.null(m)) {
        message("Returning cached data")
        return(m)
    }
    # compute inverse, cache, and return
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}
