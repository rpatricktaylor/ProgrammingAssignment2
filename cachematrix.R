## Cached Matrix Functions
## 
## These functions allow us to create an inverse-caching
## matrix object and calculate an inverse for the matrix
## with the ability to retrieve a cached result for the inverse
## if it has been computed previously


## makeCacheMatrix
##
## Create an inverse-caching matrix object -- returns
## a list containing four functions:
##    get()           - returns the matrix
##    set(y)          - sets the matrix to y (resets cached inverse)
##    getInv()        - returns the inverse of the matrix
##    setInv(inverse) - sets the cached matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## cacheSolve
##
## Calculates the inverse for a matrix contained in an 
## inverse-caching matrix object. 
## If an inverse has already been computed previously, 
## it simply returns the previously computed result.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)){
    message("Using cached inverse")
  } else {
    if(nrow(x$get()) != ncol(x$get())) {
      message("Cannot compute inverse for non-square matrix")
      inv <- NULL
    } else {
      inv <- solve(x$get())
    }
    x$setInv(inv)
  }
  return(inv)
}
