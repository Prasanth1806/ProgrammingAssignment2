## makeCacheMatrix: This function creates a matrix that can cache its inverse

##cacheSolve: This function computes the inverse of the matrix returned by 
##           makeCacheMatrix. If the inverse is already computed then it 
##           retrieves the inverse from the cache.


## makeCacheMatrix: 
## 1. set the value of the matrix.
## 2. get the value of the matrix.
## 3. set the value of the inverse.
## 4. get the value of the inverse.

makeCacheMatrix <- function(x = matrix()){
  
  i <- NULL
  
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  
  get <- function()x
  setInverse <- function(inverse)i <<- inverse
  getInverse <- function()i 
  
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## cacheSolve:
## Computes the inverse of the matrix returned from the above function.
## If the inverse has already been computed, then it gets the inverse from
## the cache and skips the computation.
## Otherwise it computes the inverse of the matrix and sets the value of the
## inverse in the cache via the setinverse function.
## Solve() function is used here for computing the inverse.

cacheSolve <- function(x, ...){
  
  i <- x$getInverse()
  
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  
  m <- x$get()
  i <- solve(m,...)
  x$setInverse(i)
  i
  
}
