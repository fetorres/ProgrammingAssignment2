## These functions are for caching and using a matrix inverse.
## The way they work is like this:  
##
## 1.  First execute 
##        m <- makeCacheMatrix()
##     m is now a special "matrix" object for caching inverses.
## 2.  Execute m$set(M) to cache M in m.  The inverse is not calculated yet.
## 3.  Execute "M_inverse <- cacheSolve(m)" whenever the inverse of M is needed.
##     The first time the inverse will be calculated, and after that the cached
##     version will be used.
## 4.  If M changes, rerun m$set(M) to put the new version of M in the cache and clear
##     the old inverse.  Otherwise it will continue to use the old version of the inverse.

#  makeCacheMatrix sets up the special "matrix" object that gives access to cached matrix inverses.

makeCacheMatrix <- function(x = matrix()) {
  x_inverse <- NULL
  set <- function(y) {
    x <<- y
    x_inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(Minverse) x_inverse <<- Minverse
  getinverse <- function() x_inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#  cacheSolve first looks if the inverse of the matrix of interest has been calculated.  If it has
#  been calculated, the cached inverse is returned.  Otherwise, the function gets the cached matrix,
#  calculates its inverse, and caches the inverse using "setinverse".  The inverse is returned. 
#
#  Precondition:  This function assumes m$set was previously executed for the latest value of the
#                 matrix of interest

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  x_inverse <- x$getinverse()
  if(!is.null(x_inverse)) {
    message("getting cached data")
    return(x_inverse)
  }
  M <- x$get()
  x_inverse <- solve(M)
  x$setinverse(x_inverse)
  x_inverse
}
