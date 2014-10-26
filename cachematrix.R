## cacheMatrix
## 10/26/2014
## Functions to cache the inverse of a matrix to save on time-consuming 
## computation. The first function creates a "matrix" object that can cache its inverse.
## The second function checks the cache and uses the cached inverse; if not cached, 
## computes the inverse.

## first function
## makeCacheMatrix - prepare the "matrix" object
## set the cache
## get the cache
## set the inverse
## get the inverse

makeCacheMatrix <- function(x= matrix()) {

	m <- NULL

	set <- function(y) {
		x <<- y
		m <<- NULL
	}

	get <- function() x
	
	setinverse <- function(inverse) m <<- inverse

	getinverse <- function() m
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)

}



## second function
## cacheSolve - return cached inverse, if not, calculate
## check to see if getinverse is not null (i.e. already calculated)
## if not null, return cached inverse;
## otherwise, solve() to capture inverse.

cacheSolve <- function(x, ...) {

	m <- x$getinverse
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m

}
