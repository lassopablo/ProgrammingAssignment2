## Put comments here that give an overall description of what your
## functions do

## This function alows to create a cacheMatrix object to use with the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
  #initialize inv variable
  inv<-NULL
  #define setters and getters methods for de data matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  
  #define setters and getters methods for the inverse variable
  setinverse <- function(inverse) inv <<- inverse
  getinverse<-function() inv
  
  #list of methods to return
  list(set = set, get = get,
       setinverse=setinverse,
       getinverse=getinverse)
  
}


## This function check if exists a cached value of the inv. 
## If inv cached value doesnot exists, it calculate an return it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #check if inv value exists
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    #if exists, return it as is
    return(inv)
  }
  #If cached inf does not exist, calculate, cache it and return it
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  #return cached value
  inv
}
