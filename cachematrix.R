## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) m <<- inv
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m <- x$getInverse() #query the x matrix cache 
  if(!is.null(m)) {   #if there is a cache
    message("getting cached data")  #just return the cache, no computation needed
    return(m)
  }
  data <- x$get()    #if there's no cache
  m <- solve(data, ...)    #we actually compute inverse here
  x$setInverse(m)       #save the result back to x's cache
  m       ## Return a matrix that is the inverse of 'x'
}
