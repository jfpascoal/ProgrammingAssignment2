## makeCacheMatrix generates a cache of the input matrix, its inverse (after the
## first calculation) and a list of handler functions: mat$get() and mat$set()
## respectively retrieve and set the cached input matrix; mat$get.inv() and 
## mat$set.inv() respectively retrieve and set the cached inverse of the matrix.


makeCacheMatrix <- function(mat = matrix()) {
     inv <- NULL
     if(!is.matrix(mat)) mat = matrix()

     set <- function(y) {
          if(is.matrix(y)) mat <<- y
          else mat <<- matrix()
          inv <<- NULL
     }
     get <- function() mat
     set.inv <- function(invMat) inv <<- invMat
     get.inv <- function() inv
     
     list(set = set,
          get = get,
          set.inv = set.inv,
          get.inv = get.inv)
}


## cacheSolve calculates and caches, or retrives the inverse of the matrix
## cached in makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     inv <- x$get.inv()
     if(is.null(inv)) {
          mat <- x$get()
          inv <- solve(mat)
          message('Caching inverse...')
          x$set.inv(inv)
     }
     inv
}
