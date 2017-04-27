## Programming Assignment 2 - R Programming
##
##
## The two functions coded below is to create a matrix,
## inverse it and store the inversed matrix in a cache. 
## Matrix inversion is usuallyvery computationally intensive - especially for large
## size matrices.  Sometimes in code (and especially in loops),
## the inverse of a matrix need only be computed once.  
## To avoid recomputing the inverse and generating the
## same result repeatedly, we can simply compute the
## result once.  If you try to recompute the inverse again,
## we have already computed this already and so we should
## just return this pre-computed result.
##

## makeCacheMatrix:
## This function is used to create the matrix
## that will be inversed in the next function below.
## The input for this function a variable of matrix type

makeCacheMatrix <- function(x = matrix()) {
  # Following the same format as the assignment example
  # Creating a makeCacheMatrix object will consist of
  # four functions encapsulated in a list
  # 1. set the matrix
  # 2. get the matrix
  # 3. set the inverse of the matrix
  # 4. get the inverse of the matrix
  
  m <- NULL # Initialization - sets the value of m to NULL (provides a default if cacheSolve has not yet been used)
  y <- NULL # Initialization - sets the value of y to NULL (provides a default if cacheSolve has not yet been used)
  setmatrix <- function(y) { #set the value of the matrix
    x <<- y ## caches the inputted matrix so that cacheSolve can check whether it has changed (note this is within the setmatrix function)
    m <<- NULL # # sets the value of m (the matrix inverse if used cacheSolve) to NULL
  }
  
  getmatrix <- function() x
  setinverse <- function(solve) m <<- solve
  
  getinverse <- function() m
  # Parts removed
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve:
## The cachesolve function will inverse the 
## matrix created above and cache it
##
## Subsequent call to the same matrix will 
## return the cached matrix computed by cachesolve 
## with requiring to regenerate the inverse matrix
##

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # Get the current state of the inverse and see if it
  # has been cached already
  inv <- x$getinverse()
  
  # If cached matrix is available
  if(!is.null(inv)) {
    
    message("Getting cached matrix")
    return(inv)
  }
  # else retrieve and inverse the matrix
 
  data <- x$getmatrix()
  
  # Find the inverse
  inv <- solve(data, ...)
  
  # Cache this result in the object
  x$setinverse(inv)
  
  # Return this new result
  inv    
  
  
}
