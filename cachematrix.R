## makeCacheMatrix function will create a Matrix, set the of matrix and get/set matrix inverse
## cacheSolve function checks the cache to see if there's already a cached value, if there is one, returns the value
## otherwise computes the inverse of the matrix, caches it and returns the inverse matrix

## function to set matrix value, get matrix value, set matrix mean, get matrix mean  

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y) {
      x <<- y
      m <<- NULL
   }
   get <- function() x
   setinverse <- function(inverse) m <<- inverse
   getinverse <- function() m
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## function to inverse the matrix
## if there's a cached value, return that 
## otherwise compute inverse and set it in cache

cacheSolve <- function(x, ...) {
   m <- x$getinverse()
   if(!is.null(m)) {
      message("getting cached data")
      return(m)
   }
   data <- x$get()
   m <- solve(data, ...)
   x$setinverse(m)
   m
}



