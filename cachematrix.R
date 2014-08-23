## Put comments here that give an overall description of what your
## functions do

## This function sets the value of the base matrix, gets the  base matrix
## sets the inverse matrix, and gets the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL # variable to store inverse matrix cleared to null on inialization
      set <- function(y) { #function to set base matrix
            x <<- y # x contains base matrix set to value provided in call
            i <<- NULL # inverse matrix reset to null when new base matrix set
      }
      get <- function() x # returns base matrix 
      setinverse <- function(inverse) i <<- inverse #sets inverse matrix
      getinverse <- function() i # returns inverse matrix 
      list(set = set, get = get,    # returns available calls with new object
           setinverse = setinverse, # creation
           getinverse = getinverse)
}


## Checks to see if the inverse has already been computed.  If it has been
## calculated it i retrived from the cache.  If it has not been calculated it
## alculates the the mean of the matrix created with the makeCacheMatrix function
## and then caches the result.

cacheSolve <- function(x, ...) {
      i <- x$getinverse() #gets inverse to see if inverse already computed
      if(!is.null(i)) { #if not null it has already been computed
            message("using cached inverse")
            return(i) # return cached inverse matrix
      }
      data <- x$get() # get base matrix
      i <- solve(data, ...) # compute inverse matrix
      x$setinverse(i) # store inverse matrix in cache
      i # return inverse matrix
}
