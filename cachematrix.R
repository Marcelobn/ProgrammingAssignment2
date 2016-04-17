## These functions calculate the inverse of matrices. 
## They save time by using cached data from previous calculations.

## 

makeCacheMatrix <- function(x = matrix() ){
  
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) I <<- Inverse
  getInverse <- function() I
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## Creates a list to
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
##         which will be an input to the next function

cacheSolve <- function(x, ...){
  I <- x$getInverse()
  if(!is.null(I)){
    message("Getting Cached Data")
    return(I)
  }
  data <- x$get()
  I <- solve(data, ...)
  x$setInverse(I)
  I
}

## Returns the inverse of a matrix