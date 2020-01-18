## Put comments here that give an overall description of what your
## functions do

## My objective in this assingment is to write and execute two functions, namely, 
## makeCacheMatrix and cacheSolve that cache the inverse of a matrix

## Write a short comment describing this function

## The function, makeCacheMatrix is a function which creates a special matrix object that can 
## cache its inverse for the input,  no errors!

makeCacheMatrix <- function(x = matrix()) {
  invmat = NULL
  set = function(y) {
    x <<- y
    invmat <<- NULL
  }
  get = function() x
  setinvmat = function(inverse) invmat <<- inverse
  getinvmat = function() invmat
  list(set = set, get = get, setinvmat = setinvmat, getinvmat = getinvmat)

}


## Write a short comment describing this function

## cacheSolve is a function which computes the inverse of the special matrix 
## returned by makeCacheMatrix computed above. If the inverse has already been calculated 
## without a change, then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv = x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data = x$get()
  inv = solve(data)
  x$setInverse(inv)
  inv      
}

