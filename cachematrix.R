##  Matrix inversion is usually a costly computation and there may be some benefit 
##  to caching the inverse of a matrix rather than compute it repeatedly 
##   My assignment is to write a pair of functions that cache the inverse of a matrix.

##The first function, makeCacheMatrix creates a special "matrix",
##which is really a list containing a function to

##set the value of the matrix
##get the value of the matrix
##set the value of the inverse of matrix
##get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,get = get,setinverse = setinverse,getinverse = getinverse)
  
  
}


##  The following function calculates the INVERSE of the special"MATRIX" created with the above function.
##  However, it first checks to see if the inverse has already been calculated.
##  If so, it gets the inverse from the cache and skips the computation. 
##  Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache 
##  via the setinverse function.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
  
}


