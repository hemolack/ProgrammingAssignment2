## This pair of functions demonstrates caching of expensive calculations using
## R's lexical scoping and the special <<- operator

## makeCacheMatrix will take a matrix as an argument and acts as an object to
## store the matrix as well as the calculated inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() { x }
  setinverse <- function(inverse) {
    i <<- inverse
  }
  getinverse <- function() { i }
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function takes the makeCacheMatrix function as an argument and returns
## the iverse of the matrix contined therein.  If the inverse has already been
## calculated it will return the cached copy, otherwise it will calculate it
## and cache the result

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("Inverse is cached ... returning cached copy")
    return(i)
  }
  message("Inverse is NOT cached ... returning calculated inverse")
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
