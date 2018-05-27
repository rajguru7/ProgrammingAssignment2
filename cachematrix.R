## These functions compute the inverse of a matrix depending on 
## whether it has already been calculated or not.

## makeCacheMatrix function creates a new object which has functions 
## set,get,getinv,setinv and values-x,m


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve takes as argument the new object created by 
## makeCacheMatrix. It finds the iverse of the argument depending
## on whether it has already been calculated or not.

cacheSolve <- function(x) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m       ## Return a matrix that is the inverse of 'x'
}
