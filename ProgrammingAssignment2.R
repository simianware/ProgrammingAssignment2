library(matlib)

makeCacheMatrix <- function(x = matrix()){
  #' Create a cacheable matrix object. The object can be interacted with in the
  #' following ways.
  #' 
  #' $set change the value of the matrix to a new matrix. Will also clear the 
  #' cached inverse.
  #' 
  #' $get retrieves the current matrix represented stored by this object.
  #' 
  #' $setinverse takes a matrix as input. Caches this value as the inverse of the
  #' matrix retrieved by $get.
  #' 
  #' $getinverse returns the cached inverse if present, NULL otherwise.
  #'
  #' @param x a matrix to be converted into a cachabele matrix object.
  #'
  #' @return a cacheable matrix.
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  #' Returns the inverse of a cacheable matrix object. Will return the cached
  #' value if present. Otherwise it will calculate the inverse, cache it, and
  #' return the value. If the matrix is not invertable will throw an error.
  #'
  #' @param x must be a cacheable matrix object created by makeCacheMatrix
  #' 
  #' @return The inverse of \code(x$get)
  #'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix <- x$get()
  inv <- Inverse(matrix)
  x$setinverse(inv)
  inv
}