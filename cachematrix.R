## The functions below will calculate the inverse of matrix. 
## First, the function will check if the inverse of the input is cached and will
## fetch it without any computation. If the inverse of the input is not cached
## then the function will compute it and cache it for the future.

## The function below takes a matrix as an argument and contains 4 sub functions
## set will set the matrix in cache, get will get the matrix
## setinverse will set the inverse in cache
## getinverse will return the inverse

makeCacheMatrix <- function(x = matrix()) {
  z <- NULL 
  set <- function(y = matrix ()) {
    x <<- y
    z <<- NULL
  }
  
  get <- function() x 
  setinverse <- function(inverse) z <<- inverse 
  getinverse <- function () z
  list(set=set, 
       get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
  
}


## The function below will take a function as an argument
## It will check if the inverse exist in cash, print a message and returns it
## If not, it will get the matrix, inverse it and set the inverse in cache
## At the end it will print the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  z <- x$getinverse()
  
  if(!is.null(z)) {
    message("Getting cashed inverse matrix")
    return(z)
  }
  
  data <- x$get()
  z <- solve(data)
  x$setinverse(z)
  z
  
}
