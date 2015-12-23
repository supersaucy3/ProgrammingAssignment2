## Put comments here that give an overall description of what your
## functions do

+  ## Funtions set a matrix, get the matrix, set the inverse matrix, and gets the inverse matrix which is cachesolve()
    
## Write a short comment describing this function
+### Function 1: makeCacheMatrix() - This function creates a special matrix object that cache's its inverse.
    
    makeCacheMatrix <- function(x = matrix()) {
        
        +  inv = NULL
        +  set = function(y) {
            +    x <<- y
            +    inv <<- NULL
            +  }
        +  get = function() x
        +  setinv = function(inverse) inv <<- inverse 
        +  getinv = function() inv
        +  list(set=set, get=get, setinv=setinv, getinv=getinv)
    }
    
    
## Write a short comment describing this function
    
## Function returns the cached inverse of the special matrix above
## Assuming nothing has changed from the matrix above then cachesolve returns the inverse from the cache.
    
    cacheSolve <- function(x, ...) {
        +  inv = x$getinv()
        +  if (!is.null(inv)){
            +  message("getting cached data")
            +   return(inv)
            +  }
        
        +  mat.data = x$get()
        +  inv = solve(mat.data, ...)
        +  x$setinv(inv)
        +  
            +  return(inv)  
    }
    
