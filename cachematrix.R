## These are two functions focused on 1) building an object with ability to
## set the value of a given matrix, get its value, set its inverse, and get its
## inverse; and 2) get the inverse of a matrix defined as one of these objects by
## returning the set value or by computing it.

## The makeCacheMatrix function has the ability of setting the value of a matrix,
## getting its value, setting the value of its inverse, and getting the value of 
## its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inversa) inv <<- inversa
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The cacheSolve function gets the inverse of a matrix built as a makeCacheMatrix
## object by returning the stored value or by computing it using solve().

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
