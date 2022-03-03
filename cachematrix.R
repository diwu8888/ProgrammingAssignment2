## I wrote 2 functions that cache the inverse of a matrix.

## The first function creates a special "matrix" object, computes its inverse, and can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
         myinverse <- NULL
  set <- function(y) {
    x <<- y
    myinverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) myinverse <<- inverse
  getinverse <- function() myinverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The second function computes the inverse of the matrix from the first function. 
## If the inverse has already been computed, then this function retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        myinverse <- x$getinverse()
  if(!is.null(myinverse)) {
    message("getting cached data")
    return(myinverse)
  }
  data <- x$get()
  myinverse <- solve(data, ...)
  x$setinverse(myinverse)
  myinverse
}
