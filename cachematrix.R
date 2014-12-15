## functions to create a special matrix which can cache it's inverse, 
## if that has been calculated, and do the calcuation and/or return the cached value


## create a new matrix object, with cache ability and accessor functions

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL #place holder for solve/inverse

  # create accessor functions
  set <- function(y) { 
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  
  # return named list of accesor functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## calulate and/or access the solved matrix in the object type above

cacheSolve <- function(x, ...) {         ## Return a matrix that is the inverse of 'x'
  i <- x$getinv() # get the cached inverse
  if(!is.null(i)) { # if it's not null, return it
    message("getting cached data")
    return(i)
  }
  
  # otherwise calculate, store, then reurn it
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
