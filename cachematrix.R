## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y    #sets a matrix to a function set
    m <<- NULL #<<- refers to m in parent environment; 
  }            #m is again set null here and y is assigned to x created/called in parent env if the matrix is created using set function
  get <- function() x   #gets the matrix to a function get
  setInverse <- function(inv) m <<- inv  #sets the inverse of a matrix to m in parent env
  getInverse <- function() m ##gets the inverse matrix m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse) #all the functions are called using list
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m <- x$getInverse() #query the x matrix already cached using makeCacheMatrix function
  if(!is.null(m)) {   #if the inverse is already created
    message("getting cached data")  #just return the message, no computation needed
    return(m)
  }
  data <- x$get()    #if there's no cache
  m <- solve(data, ...)    #we actually compute inverse here
  x$setInverse(m)       #save the result back to x's cache
  m       ## Return a matrix that is the inverse of 'x'
}
