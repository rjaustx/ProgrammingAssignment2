## makeCacheMatrix and cacheSolve matrix are pair of functions to compute matrix inverses 
## and cache the results to save computation time

## makeCacheMatrix operation:
##   - caches a matrix and its inverse
##   - gets the cached version of the matrix and inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <- y
    inv <- NULL
  }
  get <- function() x
  setinv <- function(invmtrx) inv <- invmtrx
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve operation:
##    - will check to see if cached version of matrix exists in makeCacheMatrix
##    - if exists, will return cached version
##    - if it doesn't exist, calculates inverse and caches inverse

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(x)) {
    message("getting cached data")
    return(inv)
  }
  mtrx <- x$get()
  inv <- solve(mtrx, ...)
  x$setinv(inv)
  inv
}

