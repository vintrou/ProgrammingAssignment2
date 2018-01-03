## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix : creates a special "matrix" object
##                   that can cache its inverse.
## input : x square invertible matrix
## outputs : a list containing functions that 1) set matrix, 2) get the matrix
##           3) set the inverse matrix and 4) get the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve: computes the inverse of the special
##             "matrix" returned by `makeCacheMatrix` above
##
## input : x output of makeCacheMatrix
## output : inverse of the original matrix use in makeCacheMatrix

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  matrice <- x$get()
  inverse <- solve(matrice, ...)
  
  x$setinverse(inverse)
  ## Return a matrix that is the inverse of 'x'
  inverse
  
}
