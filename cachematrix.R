## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it repeatedly

## This function creates a special "matrix" object that can cache its inverse

## Define the function that expects a matrix as argument and 
## intialize the argument with an empty matrix
makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize the place holder for Inverse matrix with NULL
  i <- NULL
  
  ## Define the setter function
  set <- function(y) {
    
    ## Assign the input value to the empty matrix and 
    ## store it in the parent environment
    x <<- y
    
    ## Reset the value of Inverse matrix with NULL and
    ## store the same in the parent environment
    i <<- NULL
  }
  
  ## Define the getter for the input matrix from parent environment
  get <- function() x
  
  ## Define the setter for the Inverse matrix into the parent environment
  setinv <- function(inv) i <<- inv
  
  ## Define the getter for the Inverse matrix from the parent environment
  getinv <- function() i
  
  ## Form a list with the above four functions and 
  ## assigning names to each of those elements (functions) of the list
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ## Get the value of the input matrix which was passed on to the 
  ## makeCacheMatrix function and assign it into the valiable called 'data'
  data <- x$get()
  
  ## Use R function 'solve' to calculate the inverse of the matrix and
  ## assign the same to 'i'
  i <- solve(data, ...)
  
  ## Store the Inverse matrix value into the parent environment for repeatative use
  x$setinv(i)
  
  ## Return the Inverse matrix value to the calling environment
  i
  }
