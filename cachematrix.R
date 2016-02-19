## This file contains a pair of functions that use caching to speed up 
## a potentially costly operation of of inverse of a matrix


## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinverse <- function(invrs) inv <<- invrs
    getinverse <- function() inv
    ##return a list containing functions to: 
    list(set = set, get = get, ##*set/get the value of the matrix
         setinverse = setinverse, ##*set the value of the inverse
         getinverse = getinverse) ##*get the value of the inverse

}


##Computes or retreave from cache the inverse of the self-caching "matrix" object. 

cacheSolve <- function(x, ...) {
      ## see if the inverse of'x' has already been calculated
      inv <- x$getinverse()  
      if(!is.null(inv)) {
        ## get the inverse from the cache and skips the computation
        message("getting cached data")
        return(inv)
      }
      data <- x$get()
      ## calculate the inverse of the 'x' 
      inv <- solve(data, ...)
      ##set the value of the inverse matrix in the cache
      x$setinverse(inv)
      ## Return a matrix that is the inverse of 'x'
      inv  
       
}
