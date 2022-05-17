## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # Set the inverse variable to null
  inverse <- NULL
  # Declare a set function which sets the internal matrix x to the argument y
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  # Declare a get function which returns the internal matrix x
  get <- function() x
  # Declare a setinverse function which sets the internal inverse matrix to the argument y
  setinverse <- function(i) inverse <<- i
  # Declare a getinverse function which returns the internal inverse matrix
  getinverse <- function() inverse
  # Return a list of all four functions 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x) {
  # Retrieve the cached inverse matrix from the special "matrix" of argument x 
  inverse <- x$getinverse()
  # If there is a cached inverse matrix, retrieve that
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  # If there is no cached inverse matrix, do the following .. 
  } else {
    # Retrieve the original matrix
    data <- x$get()
    # Calculate the inverse of that matrix
    inverse <- solve(data)
    # Set the cached inverse to that inverse
    x$setinverse(inverse)
    # Return the inverse 
    return(inverse)
  }
}

## Code for testing:

# testMatrix <- matrix(1:4,2,2)
# testCachedMatrix <- makeCacheMatrix(testMatrix)

# outputFirst <- cacheSolve(testCachedMatrix)
# outputSecond <- cacheSolve(testCachedMatrix)
