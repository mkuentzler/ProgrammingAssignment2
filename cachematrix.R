## makeCacheMatrix(M) creates a matrix object that stores an input matrix
## M and can cache its inverse.
## cacheSolve(x) returns the inverse of such a matrix object. It the
## inverse has been computed before, it is retrieved from the cache. If not,
## it is computed and cached.

## Given a matrix M, makeCacheMatrix(M) creates an object x that
## stores the matrix itself and is able to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    # inv caches the inverse. On initialization, no inverse has
    # been computed, and inv is initalized to NULL.
    inv <- NULL
    
    # set is used to store a new matrix in the matrix object makeCacheMatrix.
    # If a new object is stored, its inverse has not yet been computed, and
    # inv is re-set to NULL.
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # get returns the matrix stored in the makeCacheMatrix object.
    get <- function() x
    
    # If an inverse has been computed, it can be stored within inv using
    # setinverse.
    setinverse <- function(inverse) inv <<- inverse
    
    # A stored inverse can be retrieved from the makeCacheMatrix object using
    getinverse <- function() inv
    
    # the object is returned as a list containing the four functions defined
    # and described above.
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve(x) returns the inverse of a matrix M stored in a
## makeCacheMatrix x via 'x <- makeCacheMatrix(M)'. If the
## inverse of M has not yet been computed, it is cached. If it
## has been computed before, it is retrieved from cache, and a
## message is generated.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # Get the current cached inverse (which is null if nothing)
    # has been cached.
    inv <- x$getinverse()
    
    # If it is not null, the inverse has been computed before
    # and cached. The cache value can therefore be returned,
    # together with a message.
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # Else, the inverse has not yet been cached and has to be
    # computed. Get the full matrix from x,
    data <- x$get()
    # and use solve() to compute its inverse.
    inv <- solve(data, ...)
    # Then, cache the inverse and return it.
    x$setinverse(inv)
    return(inv)
}
