## Put comments here that give an overall description of what your
## functions do


## Make a special matrix, calculate its inverse and if inverse is already calculated 
## then retrieve the inverse from the cache



## Write a short comment describing this function


##This function creates a matrix object that can cache its inverse



makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                            
  set <- function(y) {                   
    x <<- y    
    inv <<- NULL 
  }
  get <- function() x 
  
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv                
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
  
}


## Write a short comment describing this function

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated, and original matrix is not changed
## then cacheSolve will retrieve the inverse from the cache


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
