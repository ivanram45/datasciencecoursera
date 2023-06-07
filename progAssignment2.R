## create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL  # reset the inverse cache when the matrix is updated
  }
  
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
  
  getinverse <- function() inv
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## compute the inverse of a matrix using caching
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  # if the inverse is already cached, retrieve and return it
  if (!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  # if the inverse is not cached, compute it using solve()
  data <- x$get()
  inv <- solve(data, ...)
  
  # cache the computed inverse
  x$setinverse(inv)
  
  # return the inverse
  inv
}

# Created a matrix
myMatrix <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
print(myMatrix)

# Created a cache matrix object using makeCacheMatrix
cachedMatrix <- makeCacheMatrix(myMatrix)

# Computed the inverse using cacheSolve
inverseMatrix <- cacheSolve(cachedMatrix)
print(inverseMatrix)
