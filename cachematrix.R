## The following functions cache and compute the 
## inverse of special "matrix" object that can be cached.


## This function creates a special "matrix" object
## that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(argu) {
			x <<- argu;
			inverse <<- NULL;
		}
	get <- function() return(x);
	setinv <- function(inv) inverse <<- inv;
	getinv <- function() return(inverse);
	return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}



## This function computes the inverse of the special
## "matrix" object returned by "makeCacheMatrix" above. If the
## inverse had been calculated already and the resultant matrix has 
## not changed, then "cacheSolve" should retrieve the inverse from cache.

cacheSolve <- function(x, ...) {
	inverse <- x$getinv()
	if(!is.null(inverse)) {
		message("Cached data : ")
		return(inverse)
		}
	data <- x$get()
	invserse <- solve(data, ...)
	x$setinv(inverse)
	return(inverse)
}
