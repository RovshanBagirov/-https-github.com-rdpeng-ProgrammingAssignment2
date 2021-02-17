
makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

## Functions to find the Cache inverse of a matrix


## Creates a matrix that we will cache inverse of

makeCacheMatrix <- function( matrix1 = matrix() ) {
  i <- NULL
  ## We set the matrix
  set <- function( matrix ) {
    matrix1 <<- matrix
    i <<- NULL
  }
  ##We get the matrix
  get <- function() {
    matrix1
  }
  
  ## Here we invert the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## We get the inverse of our matrix
  getInverse <- function() {
  
    i
  }
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
  
  
  matrix1 <- x$getInverse()
  
  if( !is.null(matrix1) ) {
    message("getting cached data")
    return(matrix1)
  }
  
  ## Get the matrix
  data <- x$get()
  
  ## Here by using the matrix multiplication method we find the inverse
  matrix1 <- solve(data) %*% data
  
  ## Set the inverse to the object
  x$setInverse(m)
  
  matrix1
}
