## Purpose of these functions is to be able to cache the value if inverse of a
## matrix so we do not have to compute it again if the value of matrix has not
## changed.


## makeCacheMatrix(x) creates a list containing the information about a matrix
## x and its inverse(if it has been already computed).

makeCacheMatrix <- function(x = matrix()) {
      I <- NULL
      set <- function(y) {
            x <<- y
            I <<- NULL
      }
      get <- function() x
      setinv <- function(inverse) I <<- inverse
      getinv <- function() I
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}

## cacheSolve(x) computes the inverse of a matrix x within the list created by
## function makeCacheMatrix(x) and caches the result if the inverse has not
## been computed before. Otherwise it does not compute anything but just
## returns the inverse.

cacheSolve <- function(x, ...) {
      I <- x$getinv()
      if(!is.null(I)) {
            message("getting cached data")
            return(I)
      }
      matrix <- x$get()
      I <- solve(matrix, ...)
      x$setinv(I)
      I
}