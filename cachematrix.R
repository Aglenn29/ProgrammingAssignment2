## writing a pair of functions that cache the inverse of a matrix
## This functions should create a special matrix object that can cache the inverse

## creating a cache for the inverse 

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   set <- function(y){
     x <<- y
     inv <<- NULL
   }
   get <- function() x
   setInverse <- function(solveMatrix) inv <<- solveMatrix
   getInverse <- function() inv
   list(set = set, get = get, setInverse = setInverse, getInverse =getInverse)
}


## This functions computes the inverse of the special Matrix returned by makecacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}

