## My Text Here


makeCacheMatrix <- function(x= matrix()) {

	m <- NULL

# set the value of the cache
	set<- function(y) {
		x <<- y
		m <<- NULL
	}

# get the value of the vector
	get <- function() x
	
#set the value of the mean
	setinverse <- function(inverse) m <<- inverse

#get the value of the mean
	getinverse <- function() m
	list(set = set, get = get,
		setmean = setmean,
		getmean = getmean)

}



## second function
#Example caching mean of a vector - part 2 - caching the mean
# the function first checks to see if the mean has ALREADY been calculated (emphasis mine)
# if so, it gets the mean from the cache and skips the computation 
# if not, it performs the calculation and sets the value of the mean
# in the cache via setmean



cacheSolve <- function(x, ...) {

	m <- x$getinverse
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setmean(m)
	m

}
