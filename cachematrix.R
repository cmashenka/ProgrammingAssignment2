#############################################################################
## Matrix inversion is usually a costly computation and there may 
## be some benefit to caching the inverse of a matrix rather than compute it
## repeatedly. The following two functions are used to cache the inverse
## of a matrix.
#############################################################################
## makeCacheMatrix creates a list containing a function to
## a. set the value of the matrix
## b. get the value of the matrix
## c. set the value of inverse of the matrix
## d. get the value of inverse of the matrix
############################################################################
## step a. set the value of the matrix
makeCacheMatrix <- function(x = matrix()) {
  invrs <- NULL
  set <- function(y) {
    x <<- y
    invrs <<- NULL
  }
  ## step b. get the value of the matrix
  get <- function() x
  ## step c. set the inverse of the matrix
  setinverse <- function(inverse) invrs <<- inverse
  ## step d. get the inverse of the matrix
  getinverse <- function() invrs
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

#####################################################################
## This function returns the inverse of the matrix. The function
## operates under the assumtion that the matrix is always invertable.
#####################################################################
## Step a. Checkin if the inverse has already been obtained
## Step b. if matrix inverse has been already computed, get output,
## if not get inverse of the matrix
cacheSolve <- function(x, ...) {
  invrs <- x$getinverse()
  if(!is.null(invrs)) {
    message("getting cached data.")
    return(invrs)
  }
  data <- x$get()
  invrs <- solve(data)
  ## Step c.set the inverse of the matrix 
  x$setinverse(invrs)
  invrs
}
