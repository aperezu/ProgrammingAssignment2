## Function that returns a list with 4 functions to manage with matrices and its 
## inverse
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
                x <<- y
                m <<- NULL
      }
      get <- function() x
	setinverse <- function(solve) inv <<- solve
	getinverse <- function() inv		
	list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Due to inverse calculation might be a time-consuming process, 
## this function saves the inverse matrix in cache for further consumption
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if(!is.null(inv)) {
                message("getting inverse cached data")
                return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      inv
}