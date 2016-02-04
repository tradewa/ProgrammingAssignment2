## These two functions are used to construct special "matrix" 
## and calculate its inverse
## The inverse of the matrix is cached and if the inverse is called
## for the second time (as long as the matrix is not changed) the 
## value is taken from cache so that no second computation needed
## For a big matrix, this method reduce computing time significantly

## This function takes matrix as an input 
## and outputs a list containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function(){
    x
  }
  setInverse <- function(inverse){
    inv <<- inverse
  }
  getInverse <- function(){
    inv
  }
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function calculates the inverse of the special "matrix"
## created with makeCacheMatrix function
## This function first check whether the inverse has already been calculated
## If so, the inverse value is taken from the cache and no computation is needed
## If no cached data, this function will calculate first the inverse and then outputs the result

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
