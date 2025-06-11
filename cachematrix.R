## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function


## These functions - 'makeCacheMatrix' and 'cacheSolve' - are used to cache the inverse of matrix
## 'makeCacheMatrix' function creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL 
    set <- function(y) {
      x <<- y    
      inv <<- NULL 
    }
  
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set,
           get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## Write a short comment describing this function
## The 'cacheSolve' function computes the inverse matrix, using the cache if there is

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()  
    if (!is.null(inv)) {
      message("getting cached data")  
      return(inv)             
    }
    data <- x$get()         
    inv <- solve(data, ...)
    x$setinverse(inv)       
    inv               
}
