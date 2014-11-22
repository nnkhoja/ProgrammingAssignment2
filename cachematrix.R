
## The funtion creates a special "matrix" object that can allow cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  # inv will store the inverse
  # inv is An index value of NULL is treated as if it were integer(0)
  inv <- NULL
  
  # the funtion set will be used to alter the matrix
  # it invalidates the cache, if inv is not null
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # the function get will returns the raw matrix
  get <- function() {
    x
  }
  
  # the fuction setinv will sets the inv variable
  # should be used only by cacheSolve
  setinv <- function(i) {
    inv <<- i
  }
  
  # the fuction getinv gets the cached inverse
  getinv <- function() {
    inv
  }
  
  # the list will be return as the special matrix
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)    

}


## The funtion will computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), then cachesolve will retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  # it will get the cached inverse
  inv <- x$getinv()
  
  if(!is.null(inv)) {
    # if the inverse if actually cached, just return it
    message("returning cached inverse")
    return(inv)
  }
  
  # else, calculate the inverse and cache it
  mtrx <- x$get()
  inv <- solve(matr, ...)
  x$setinv(inv)
  
  ## Return a matrix that is the inverse of 'x'
  return(inv)
        
}
