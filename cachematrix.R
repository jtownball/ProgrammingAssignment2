## makeCacheMatrix and cacheSolve can be used together to efficently calculate the inverse of
## an invertible matrix. First create a matrix inverse cache with makeCacheMatrix and then pass 
## that argument to the cacheSolve function. This will 

## This function will return a list with the functions for set, get,
## setinv (for setting the inverse of the input matrix)
## getinv (for getting the inverse of the matrix)
## once the inverse has been computed, it can be cached here for future use
makeCacheMatrix <- function(x = matrix()) 
{
  m <- NULL;
  set <- function(y) 
  {
    x <<- y;
    m <<- NULL;
  }
  get <- function() x
  setinv <- function(inv) m <<- inv;
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function will return the inverse of the input matrix
## The input matrix should be created with the makeCacheMatrix
## If the inverse solution has been never been calculated it will
## store the inverse matrix in the makeCacheMatrix, else it will
## retrive the previously computed values.
cacheSolve <- function(x, ...) 
{
  m <- x$getinv();
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
