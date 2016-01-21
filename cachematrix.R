## 'makeCacheMatrix' and 'cacheSolve' are used to create cached
## matrices and relate them to their cached inverses to recall
## them more quickly.
##
## 'makeCacheMatrix' creates functions tied to the matrix 'x'
## that can be called to retrieve & store a matrix's inverse
##
## 'cacheSolve' leverages the functions defined in 'makeCacheMatrix'
## to either recall the cached inverse or solve for the inverse & cache it


## 'makeCacheMatrix' creates sub-functions that can be called to
## quickly retrieve & store the inverse of an input matrix 'x'
##
## 'set' stores the matrix within the created list
## 'get' retrieves the matrix in question
## 'setinverse' caches the calculated inverse of the matrix 'x'
## 'getinverse' retrieves the cached inverse, if it exists

makeCacheMatrix <- function(x = matrix()) {
	inver <- NULL
	set <- function(y) {
			x <<- y
			inver <<- NULL
	}

	get <- function() x
	setinverse <- function(inverse) inver <<- inverse
	getinverse <- function() inver
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## 'cacheSolve' leverages the functions defined in the
## user function 'makeCacheMatrix' to either recall the
## cached inverse or solve for the inverse & cache it

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
			
	inver <- x$getinverse()
	if(!is.null(inver)) {
			message("getting cached data")
			return(inver)
	}
	data <- x$get()
	inver <- solve(data, ...)
	x$setinverse(inver)
	return(inver)
}
