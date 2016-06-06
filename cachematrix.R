## The functions makeCacheMatrix and cacheSolve are used to find and cache
## the inverse of a square invertible matrix.

## Additionally I have added logics to cache the input matrix that was last passed 
## to find the inverse and if the current matrix input is same as the one passed before
## then the solve function will not be invoked and will return the cached inverse matrix 
## from the previous computation. 

## The makeCacheMatrix function returns the cached inverse of the input square matrix to
## the cacheSolve function.

makeCacheMatrix <- function(matrix_var = matrix()) {
  
  set <- function() {
    
    j <- 0
    
    if (exists("cache_input")) {
      
      if (length(matrix_var) != length(cache_input)) {
        
        cache_input <<- matrix_var
        
        cache_inv <<- NULL    
        
      } else {
        
        matrix_var_list <- unlist(as.list(matrix_var))
        
        cache_input_list <- unlist(as.list(cache_input))
        
        for (i in 1:length(matrix_var_list)) {
          if (cache_input_list[i] != matrix_var_list[i]) {
            j <- j + 1
          }     
        }
      }
      
      if (j > 0) {
        cache_input <<- matrix_var
        cache_inv <<- NULL
      }
      
    } else {
      cache_input <<- matrix_var
      cache_inv <<- NULL
    }
    
  }
  
  get <- function() matrix_var
  
  setinv <- function(inverse) cache_inv <<- inverse
  
  getinv <- function() cache_inv
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The cacheSolve function accepts and returns the cached inverse matrix if makeCacheMatrix
## function returns any or computes the inverse of a square matrix.

cacheSolve <- function(matrix_func, ...) {
  
  ## Return a matrix that is the inverse of 'matrix_var'
  
  matrix_func$set()
  
  cache_inv <- matrix_func$getinv()
  
  if(!is.null(cache_inv)) {
    message("getting cached data")
    return(cache_inv)
  }
  
  matrix_data <- matrix_func$get()
  
  cache_inv <- solve(matrix_data)
  
  matrix_func$setinv(cache_inv)
  
  cache_inv
}
