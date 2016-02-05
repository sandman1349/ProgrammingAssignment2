## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## first develop a null vector to hold the matrix 
  inverse <- NULL       
  ## define the function which will  store the matrix 
  set <- function(y) {              
    x <<- y
    inverse <<- NULL
  }
  ## get function will retrieve our matrix
  get <- function() x           
  # Define 'setcalc' function which holds cached inverse and the 'getcalc' which will return the cached inverse
  setcalc <- function(calc) inverse <<- calc
  getcalc <- function() inverse
  list(set = set, get = get,
       setcalc = setcalc,
       getcalc = getcalc)
}


## The point of this function is to understand if the inverse of a matrix has already been calculated. 
## If it has already, then the cache value will be returned.
## Otherwise, the inverse will be calculated and stored to cache. 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inverse <- x$getcalc()
    if(!is.null(inverse)) {
      message("getting cached data")
      return(inverse)
    }
    data <- x$get()
    inverse <- solve(data,...)
    x$setcalc(inverse)
    inverse
        ## Return a matrix that is the inverse of 'x'
}
