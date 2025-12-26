
## ---------------------------------------------
## Lexical Scoping Assignment: Caching a Matrix Inverse
## Author: (Your Name)
## ---------------------------------------------

## makeCacheMatrix:
## This function creates a special "matrix" object that can cache its inverse.
## It returns a list of four functions:
##   - set():    set the value of the matrix
##   - get():    get the value of the matrix
##   - setInv(): set the cached inverse
##   - getInv(): get the cached inverse
##
## Internally, it uses lexical scoping to keep 'x' (the matrix)
## and 'inv' (its inverse) in the closure environment.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # 'inv' will hold the cached inverse (initially NULL)

  # Setter for the matrix: updates 'x' and clears cached inverse
  set <- function(y) {
    if (!is.matrix(y)) {
      stop("Input must be a matrix.")
    }
    x <<- y
    inv <<- NULL  # Invalidate cache whenever matrix changes
  }

  # Getter for the matrix
  get <- function() {
    x
  }

  # Setter for the inverse (to be called by cacheSolve)
  setInv <- function(inverse) {
    inv <<- inverse
  }

  # Getter for the inverse
  getInv <- function() {
    inv
  }

  # Return a list with the four functions
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## cacheSolve:
## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix(). If the inverse has already been calculated and the matrix
## has not changed, then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  # Try to get a cached inverse
  inv <- x$getInv()
  if (!is.null(inv)) {
    message("Getting cached inverse.")
    return(inv)
  }

  # Otherwise, compute the inverse
  mat <- x$get()

  # Basic checks
  if (!is.matrix(mat)) {
    stop("The stored object is not a matrix.")
  }
  if (nrow(mat) != ncol(mat)) {
    stop("Matrix must be square to compute an inverse.")
  }

  # Check if matrix is singular (optional: robust check via determinant)
  # Note: solve() will error if matrix is singular.
  # det_val <- det(mat)
  # if (isTRUE(all.equal(det_val, 0))) stop("Matrix is singular; inverse does not exist.")

  inv <- solve(mat, ...)  # Compute inverse
  x$setInv(inv)           # Cache the inverse
  inv
}
