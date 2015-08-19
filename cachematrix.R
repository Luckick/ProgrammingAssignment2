## The function makeCacheMatrix creates a special "matrix" object 
## that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y               # use <<- in the global enviroment
    i <<- NULL
  }
  
  # constant function, returns the matrix which need to be computed
  get <- function() x    
 
  # return the cached result
  setinverse <- function(inverse) i <<- inverse
  
  # constant function
  getinverse <- function() i
  
  # renturn a list which contains 4 elements,
  # every single element is a defined function
  list(set = set,get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}

## The function cacheSolve computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated 
## (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  
  if(!is.null(i)) {
    # i is not NULL, which means inverse has been computed
    message("getting cached data")
    return(i)
  }
  
  # inverse has not been computed
  # has to use function get to get matrix which need to compute 
  data <- x$get()
  
  # compute the inverse
  i <- solve(data, ...)
  
  # save the inverse result by using setinverse
  # the next time we want to use cacheSolve, cache has already existed
  # it can be return before if without computation
  x$setinverse(i)
  
  # return the final result
  i
}
