## Functions cache the inverse of a matrix 

## This function creates a matrix object to cache the inverse

makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function calcs the inverse of a special matrix or 
## retrieves its inverse from a cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getinverse()
  if(!isnull(m)){
    message("getting cached data")
    return(m)
    
  }
  data<- x$get()
  m<- solve(data, ...)
  x$setinverse(m)
  m
  
}