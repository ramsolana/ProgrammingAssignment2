## Put comments here that give an overall description of what your
## functions do

## This function will calculate the inverse of a given square matrix and save it to cache.
## To test the function I used matrix:
##    "matrix(c(4, 2, 7, 6), nrow = 2, ncol = 2, byrow = TRUE)"

makeCacheMatrix <- function(x = matrix()) {
  m         <- NULL # sets the value of m to NULL (Matrix has not changed)
  inversed  <- NULL # sets the value of inversed to NULL (Matrix has not been inversed yet)
  
  setmatrix <- function(y) { #set the value of the matrix
    x <<- y     ## caches the inputted matrix so that cacheSolve can check whether it has changed (note this is within the setmatrix function)
    m <<- NULL  ## sets the value of m (the matrix inverse if used cacheSolve) to NULL
  }
  setinverse <- function(m) {
    inversed <<- m ## Caches the inversed matrix
  }
  getmatrix <- function () {
    x       ## return the matrix
  }
  getinverse<-function() {
    inversed ##Returns the inversed matrix
  }

  # Return a List with the methods
   list(setmatrix = setmatrix, 
        getmatrix = getmatrix, # creates a list to house the four functions
        setinverse = setinverse,
        getinverse = getinverse)
}


## This function takes the input matrix, check if it has been inversed or not:
##  - if yes, return its inverse
##  - if not, calculate the inverse for that matrix

cacheSolve <- function (x=matrix(), ...) {
  # Need to compare matrix to what was there before!
  m <- x$getinverse() # if an inverse has already been calculated this gets it
  
  # check to see if cacheSolve has been run before.
  # Furtheremore, if the matrix has changed, its inversded variable is null, since it's not thr same matrix
  if(!is.null(m)){ 
      #parts removed
      print("This matrix has been already inversed, here is its inverse: ")
      return(m)
  }  
  # otherwise 
  print("First time this matrix is getting inversed")
  y <- x$getmatrix() # run the getmatrix function to get the value of the input matrix
  x$setmatrix(y) # run the setmatrix function on the input matrix to cache it
  m <- solve(y, ...) # compute the value of the inverse of the input matrix
  x$setinverse(m) # run the setinverse function on the inverse to cache the inverse
  m # return the inverse
}