## The function makeCacheMatrix creates a list of the following four functions:
## 1. set: Sets the value of the matrix
## 2. get: Gets the value of the matrix
## 3. setinverse: Sets the value of the inverse
## 4. getinverse: Gets the value of the inverse


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


## The function cacheSolve calculates the inverse of 'x' if it's not calculated yet.
## Otherwise, just retrieves the calculated value of the inverse of 'x' and does NOT calculate it again.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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


## Testing

## 1: Create matrix with normally-distributed random values of size 2000x2000
a <- as.matrix(replicate(2000, rnorm(2000))

## 2: Calculate inverse and storing in memory with makeCacheMatrix and cacheSolve functions              
k <- makeCacheMatrix(a)
cacheSolve(k)

## 3: Comparing execution time between the already calculated cacheSolve(k) and solve(a)

system.time(cacheSolve(k)) #Execution time: 0s.
system.time(solve(a)) #Execution time: 9.6s-9.7s approx.
