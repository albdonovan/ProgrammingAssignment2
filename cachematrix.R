## In order to calculate the inverse of a matrix, operation
## that can be very time and resource consuming depending on the dimension of
## the matrix, following are two functions that solve the problem:


## Auxiliary function that creates a list containing a matrix and its inverse and
## cache it:

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## Function that calculates the inverse of a matrix checking first if the value is cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()
	if(!is.null(i)) {
		message("getting cached data.")
		return(i)
	}
	originalmatrix <- x$get()
	i <- solve(originalmatrix)
	x$setinverse(i)
	i
}
