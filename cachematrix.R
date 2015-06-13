## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly. The following pair of functions facilitates the caching of
## the inverse of a matrix.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL # 'inverse' is the cache in which matrix inverse is stored
		
	# 'set' function to set the matrix of this special object
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	
	# 'get' function to get the matrix of this special object
	get <- function() x
	
	# 'setInverse' function to cache the inverse 
	setInverse <- function(inv) inverse <<- inv
	
	# 'getInverse' function to get the cached inverse
	getInverse <- function() inverse
	
	list(set = set, get = get, setInverse = setInverse,
		getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by 
## 'makeCacheMatrix' above. If the inverse has already been calculated (and 
## the matrix has not changed), then 'cacheSolve' should retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	
	inv <- x$getInverse()
	
	# if inverse exists return the cached data
	if(!is.null(inv))  {
		message("getting cached data")
		return(inv)	
	}
	
	# else get the matrix and compute        
	mat <- x$get()
	inv <- solve(mat)
	x$setInverse(inv)
	inv        
}
