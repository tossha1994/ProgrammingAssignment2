##This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix<-function(x=matrix()){
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setinverseM <- function(inverseM) im <<- inverseM
  getinverseM <- function() im
  list(set = set, get = get,
       setinverseM = setinverseM,
       getinverseM = getinverseM)
}

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#If the inverse has already been calculated (and the matrix has not changed),
#then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  im <- x$getinverseM()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setinverseM(im)
  im
}
