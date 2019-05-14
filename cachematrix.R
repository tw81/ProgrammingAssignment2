## Create a function to store a matrix and its inverse, 
## then create a second function which calls the first, and calcualtes the inverse

## Function stores a matrix, and its inverse

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
           x <<- y  
           m <<- NULL 
     }
     get <- function() x 
     setinv <- function(inv) m <<- inv  
     getinv <- function() m 
     list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## try to get the matrix inverse defined in 'makeCacheMatrix.
## If it is not already defined calculate the inverse, then store it 

cacheSolve <- function(x, ...) {
      # Return a matrix that is the inverse of 'x'
      m <- x$getinv()
      if(!is.null(m)) { 
          message("getting cached data")  
          return(m)
      } 
      data <- x$get() 
      m <- solve(data, ...) 
      x$setinv(m)   
      m 
}

