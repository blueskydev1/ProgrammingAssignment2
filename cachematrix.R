## Caching a matrix inversion for re-use can save on computation time.
## These two functions work togther to create a matrix object that can cache and then
## retrieve its inverse. The first creates the object, the second then retrieves the 
## cached inverse; if not already cached, computes the inverse and caches it 
## for future use.

## first function
## makeCacheMatrix prepares the matrix object

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
## cacheSolve retrieves cached inverse, if not, calculates & caches it

## check to see if getinverse is not null (i.e. already calculated)
## if not null, retrieve cached inverse;
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
