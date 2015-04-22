## Put comments here that give an overall description of what your
## functions do
##	The basic idea here is that makeCacheMatrix contains the data while cacheSolve contains the means to calculate the inverse of the matrix.

## Write a short comment describing this function
## makeCacheMatrix is a function that holds the matrix and its inverse together with some function for setting and retrieving the matrices. 
## makeCacheMatrix contains following functions:
## set(y) assigns the received matrix object y to the internal container x, since the matrix has changed the inverse is erased.
## get() simply returns the stored matrix.
## setinverse(inverse) stores the received matrix.
## getinverse() simply returns the cashed inverse of the stored matrix.
 
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get,
			setinverse = setinverse,
			getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve stores the matrix returned by makeCacheMatrix and calculates and returns the inverse of that matrix.
## First this function calls getinverse() function from makeCacheMatrix. This is to check the cached value.
## Once this value is received it checks if it is other than NULL.
## In case the value is other than NULL a message is displayed about matrix being received and the result returned.
## If the value is NULL then the matrix is received from the makeCacheMatrix object and an inverse is computed.
## When the inverse is calculated it is stored in the makeCacheMatrix and returned.

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
