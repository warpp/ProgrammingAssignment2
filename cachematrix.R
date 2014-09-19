## Programming assignment 2
## Two functions that create a matrix and cache its inverse.
## If inverse is not yet calculated then the function makes the
## calculation, else it "retrieves" the stored inverse and skips 
## the calculation.


## creates a matrix object that contains functions to set and get  
## value of matrix and set and get inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL        
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## calculates inverse of matrix created by makeCacheMatrix function. If a matrix has been changed or
## inverse is already computed, retrieves inverse from cache.


cacheSolve <- function(x, ...) {
      
      inv <- x$getinv()
      if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv
}
