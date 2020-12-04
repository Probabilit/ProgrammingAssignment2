
## R Programming: Week 3 assignment 

## This function creates a special "matrix" that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL   ## set inv as null, hold  values of matrix inverse     
  set <- function(y) { 
    x <<- y       ## value of matrix in parent environment 
    inv <<- NULL  ## reset inv to null in case of a new matrix 
  }
  get <- function() x   ## get value of matrix  
  setInverse <- function(inverse)
    inv <<- inverse    ## assign value of inv in parent environment
  getInverse <- function() inv ## get value of inv 
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse) ## to use $ operator 
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()     
  if(!is.null(inv)) {
    message("getting cached data") ## if not null, print message
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...) ## get the inverse of the matrix  
  x$setInverse(inv)
  inv 
}
