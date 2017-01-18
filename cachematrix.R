## Creating a pair of functions to cache the inverse of matix in order to avoid furture ##computations when required. This is an assignment as a part of 'R-programming course'

#setting a matrix using set function
#calling a matrix that has been created using 'get'
#storing the inverse of the matrix using 'setinverse'
#calling the inverse that has been created 'getinverse'

## This function creates a special "matrix" object that can cache its inverse.

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


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     m <- x$getinverse()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data)
     x$setinverse(m)
     m

}

