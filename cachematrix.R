## Overall description:
## Caching the Inverse of a Matrix

## The following function creates a special "matrix" object that can cache its inverse:

makeCacheMatrix <- function(x = matrix()){
  inverse <- NULL
  set <- function(m){
    x <<- m
    inverse <<- NULL
  }
  setinv <- function(i){inverse <<- i}
  get <- function() x
  getinv <- function() inverse
  list(set = set,
       get = get,
       getinv = getinv,
       setinv = setinv
       )
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix:
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache:

cacheSolve<-function(x, ...){
  inv <- x$getinv()
  if(!is.null(inv)){
    message("find cached value, return")
    return(inv)
  }
  message("no cache, calculating...")
  inv = solve(x$get(), ...)
  x$setinv(inv)
  inv
}

