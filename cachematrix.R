## MakeCacheMatrix creates a matrix-like object which caches it's own inverse.
## When cacheSolve takes a result from MakeCacheMatrix as an argument, 
## cacheSolve checks to see if the inverse has been calculated and returns the cached inverse instead of recalculating it. 

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
## set the value of the matrix
  set <- function(y) {                      
    x <<- y
    m <<- NULL
  }                                        
  get <- function() x                       ## get the value of the matrix
  setsolve <- function(solve) m <<- solve   ## set the value of the inverse
  getsolve <- function() m                  ## get the value of the inverse
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}



## This function computes the inverse of the special matrix returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {

  m <- x$getsolve()
  if(!is.null(m)) {			## If the inverse has already been calculated (and the matrix has not changed) 
    message("getting cached data")	## it retrieve the inverse from the cache and skips the computation.
    return(m)
  }
  data <- x$get()				                                                  
  m <- solve(data, ...)		        ## the solve function calculates the inverse of the special matix returned by makeCacheMatrix
  x$setsolve(m)                         ## the setsolve function sets the inverse in the cache
  m
}
