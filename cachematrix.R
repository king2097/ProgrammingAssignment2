## cahcematrix functions create and cache the inverse of a matrix. Since matrix inversion typically consumes lots of computational resources, it is beneficial to cache the inverse of a matrix instead of computing it multiple times.

## makeCacheMatrix() creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() {
                x
        }
        
        setinverse <- function(mean) {
                m <<- mean
        }
        
        getinverse <- function() {
                m
        }
                
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve() computes the inverse of the special "matrix" returned by makeCacheMatrix() above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve() should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        cachemean <- function(x, ...) {
                m <- x$getinverse()
                
                if(!is.null(m)) {
                        message("getting cached matrix")
                        return(m)
                }
                
        ## Calculates the inverse of the matrix
                data <- x$get()
                m <- solve(data, ...)
                x$setinverse(m)
                m
        }
}

