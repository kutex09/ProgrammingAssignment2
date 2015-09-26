## The overall aim of the functions below is to compute the inverse of a matrix and cache the value of 
## of the results so that if needed again, it will simply be looked in the cache rather than recomputed.
## In a nutshell the functions cache the inverse of a matrix.

## This makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## The function takes an invertible square matrix as arguement. The function essentially creates a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  #Set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #Get the value of the matrix
  get <- function() x
  #Set the inverse of the matrix
  setInverse <- function(solve) m <<- solve
  #Get the inverse of the matrix
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## This cacheSolve function calculates the inverse of the "special" matrix object created by the
## makeCacheMatrix function above. It however checks to see if the inverse has already been calculated.
## If the inverse has already been calculated, it gets that value from the cache and skips the computation.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  #check if inverse has been calculated
  #if so, pull the value from the cache and return is as the result
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #if not get the matrix and find the inverse using the solve function
  data <- x$get()
  m <- solve(data, ...) # Assign the value to m
  x$setInverse(m)
  m # return the matrix that is the inverse of "x'
}

## Example with a 2 by 2 matrix
# testmat <- matrix(c(4,3,3,2),2,2,byrow = TRUE)
#      [,1] [,2]
# [1,]    4    3
# [2,]    3    2
# mc <- makeCacheMatrix(x = testmat)
# cacheSolve(mc)

#      [,1] [,2]
# [1,]   -2    3
# [2,]    3   -4
