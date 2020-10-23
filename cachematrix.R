## Put comments here that give an overall description of what your
## makeCacheMatrix returns an object which is in essence a cachable matrix. cacheSolve
## takes a cachable matrix and inverts it if it hasn't already been cached 
## functions do

## Write a short comment describing this function
## returns a list of setters and getters for the supplied matrix, x

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) I <<- inv
  getinverse <- function() I
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## If the inverse of x is cached then cacheSolve returns the cached value, I. 
## If the inverse of x isn't cached then cacheSolve inverts x, caches I and returns I

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  I <- x$getinverse()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  I <- solve(data, ...)
  x$setinverse(I)
  I
}
