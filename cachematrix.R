## Write the following functions:
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

## Computing the inverse of a square matrix can be done with the solve function in R. For example, 
## if X is a square invertible matrix, then solve(X) returns its inverse.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv_mix <<- NULL
  set <- function(y)
  {
    x <- y
    inv_mix <<- NULL
  }
  get <-function() x
  setinv <- function(inv) inv_mix <<- inv
  getinv <- function() inv_mix
  x <<- list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(...) {
        ## Return a matrix that is the inverse of 'x'
  inv_mix <- x$getinv()
  if(!is.null(inv_mix)) {
    message("Getting cached Data")
    return(inv_mix)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
