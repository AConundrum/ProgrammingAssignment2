makeCacheMatrix <- function(cacheMM=matrix()) {
## makeCacheMatrix: creates a matrix-object that can cache its inverse.  This
## object is a list of functions that...
## 1. Caches the value of the matrix:       makeCacheMatrix$set(MATRIX)
## 2. Gets/prints the value of the matrix:  makeCacheMatrix$get()
## 3. Caches the value of the inverse:      makeCacheMatrix$setinverse(MATRIX)
## 4. Gets/prints the value of the inverse: makeCacheMatrix$getinverse()

cacheINV <- NULL              # defines parent env. variable cacheINV
                              # accessible to all 4 functions
                              # (otherwise <<- operator assigns to global)
####################################     
set <- function(MM) {
     cacheMM <<- MM           # cache to parent environment
     cacheINV <<- NULL        # cache to parent environment
                              # (erase any previous value)
}
####################################
get <- function() cacheMM     # get/print matrix
####################################
# cache result (from cacheSolve.R)
setinverse <- function(INV) cacheINV <<- INV
####################################
getinverse <- function() cacheINV  # get/print inv of matrix
####################################
list(set = set, get = get,    # output functions to list
     setinverse = setinverse,
     getinverse = getinverse)
} # function

###############################################################################
cacheSolve <- function(x, ...) {
## cacheSolve: computes the inverse of the matrix-object
## returned by makeCasheMatrix.R  If the inverse has already
## been calculated (and the matrix has not changed), then the
## cacheSolve should retrieve the inverse from the cache.
## Return a matrix that is the inverse of 'x'
     
INV <- x$getinverse()         # Ask for INV from matrix-object
if(!is.null(INV)) {           # If is has a value other than NULL.. 
     message("<cacheSolve.R>: Getting cached data")
     return(INV)              # ...return the cached value
}
data <- x$get()               # Otherwise, get matrix
INV <- solve(data, ...)       # calculate the inverse
x$setinverse(INV)             # and cache it,
INV                           # and print it to command line
} # function
