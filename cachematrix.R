## Put comments here that give an overall description of what your
## functions do
##	The basic idea here is that makeCacheMatrix contains the data while cacheSolve contains the means to calculate the inverse of the matrix.

## Write a short comment describing this function
## makeCacheMatrix is a function that holds the matrix and its inverse together with some function for setting and retrieving the matrices. 
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get,
			setinverse = setinverse,
			getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve stores the matrix returned by makeCacheMatrix and calculates and returns the inverse of that matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getinverse()
		if(!is.null(inv)){
				message("getting the inverse matrix")
				return(inv)
		}
		tmpMatrix <- x$get()
		inv <- solve(tmpMatrix)
		x$setinverse(inv)
		inv
}
