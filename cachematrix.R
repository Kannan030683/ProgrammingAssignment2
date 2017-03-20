## Put comments here that give an overall description of what your
## functions do

##These functions written in fulfillment of Coursera Data Science: R Programming 
## week 3 programming assignment


## Write a short comment describing this function
## A pair of functions that cache the inverse of a matrix.
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invmat <- NULL          ## initialize invmat as NULL; will hold value of matrix inverse 
  set <- function(y){     ## define the set function to assign new
    x <<- y               ## value of matrix in parent environment
    invmat <<- NULL       ## if there is a new matrix, reset invmat to NULL
  }
  get <- function() x     ## define the get fucntion - returns value of the matrix argument
  setInverse <- function(solutionMatrix) invmat <<- solutionMatrix ## assigns value of invmat in parent environment
  getInverse <- function() invmat    ## gets the value of invmat where called
  list(set = set, get = get, 
       setInverse = setInverse,  
       getInverse = getInverse)      ## We need this in order to referto the function with $ operator
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invmat <- x$getInverse() 
  if(!is.null(invmat)){
    message("Returning cached data")
    return(invmat)
  }
  data <- x$get()
  invmat <- solve(data)
  x$setInverse(invmat)
  invmat   
}
