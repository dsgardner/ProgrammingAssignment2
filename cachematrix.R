## Two functions that create a special object that stores a matrix and caches its inverse

## Creates a special "matrix," which is really a list containing functions to
## set and get the values of the matrix and the values of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) i <<- inverse
      getinverse <- function() i
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Calculates the inverse of the special "matrix" created by the "makeCacheMatrix" function
## First checks to see if an inverse already has been calculated, and retrieves and returns it if so
## Otherwise, it uses the solve() function on the matrix and caches the result

cacheSolve <- function(x, ...) {
	i <- x$getinverse()
      if(!is.null(i)) {
            message("getting cached inverse data...")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(i)
      return(i)	
}
