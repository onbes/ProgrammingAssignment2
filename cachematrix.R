## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInv <- function(inv) inverse <<- inv
  getInv <- function() inverse
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the 
##cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInv()
        
        if(!is.null(inverse)){
          message("getting cached inverse matrix")
          return(inverse)
        }
        
        ## Inverse is null, so get the matrix 
        invData  <- x$get()
        ## and inverse the matrix
        inverse <- solve(invData,...)
        
        #save/cache the inverse
        x$setInv(inverse)
        
        #return inverse of matrix
        inverse
}
