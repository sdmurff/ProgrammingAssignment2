## This programs consists of two functions that work together in order to cache
## the inverse of a matrix to save processing time if the inverse has already 
## been calculated. The first function creates a list composed of 4 other functions
## which do the following:
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse
## 4.  get the value of the inverse
## The second function checks to see if the inverse of a given matrix is already cached,
## if so, the inverse in returned. If not, the inverse is computed.

## This funciton takes a square matrix as an argument and outputs a list of functions.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function takes as an argument a list of functions and returns the 
## inverse of previously defined matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
