## This file contains a pair of functions that use caching to speed up 
## a potentially costly operation of of inverse of a matrix


## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
      x <<- y
      s <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) s <<- solve
    getinverse <- function() s
    ##return a list containing functions to: 
    list(set = set, get = get, ##*set/get the value of the matrix
         setinverse = setinverse, ##*set the value of the inverse
         getinverse = getinverse) ##*get the value of the inverse

}


##Computes or retreave from cache the inverse of the self-caching "matrix" object. 

cacheSolve <- function(x, ...) {
      ## see if the inverse of'x' has already been calculated
      s <- x$getinverse()  
      if(!is.null(s)) {
        ## get the inverse from the cache and skips the computation
        message("getting cached data")
        return(s)
      }
      data <- x$get()
      ## calculate the inverse of the 'x' 
      s <- solve(data, ...)
      ##set the value of the inverse matrix in the cache
      x$setinverse(s)
      ## Return a matrix that is the inverse of 'x'
      s  
       
}
