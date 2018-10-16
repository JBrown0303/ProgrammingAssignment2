## Put comments here that give an overall description of what your
## functions do

## Function that sets and returns a chaced matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
	#initialize the inverse as Null
	i <- NULL

	#create function for caching the input matrix
	set <- function(y) {
		x <<- y 		## stores input matrix y as x
		i <<- NULL		## reset the inverse to NULL
	}

	#create function for returning the cached matrix
	get <- function() x

	#create function for caching the inverse
	setInverse <- function(inverse) i <<- inverse

	#create function for returning the cached inverse
	getInverse <- function() i

	#return a list of the created sub-functions
	list(set = set, get = get, setInverse = setInverse, 
		getInverse = getInverse)
}


## Returns the inverse of the matrix. the inverse is cached after the 
## first calculation function loads inverse from cach after it has 
## been calculated
cacheSolve <- function(x, ...) {
	#pull the cached value of 'i'
	i <- x$getInverse()

	#check if cached value is NULL. If it is not, return the cached
	#value
	if(!is.null(i)){
		message("getting cached data")
		return(i)
	}

	#if there is no value for the inverse cached, pull the cached matrix
	data <- x$get()

	#then calculate the inverse
	i <- solve(data, ...)

	#then cache the inverse
	x$setInverse(i)

	#return the inverse
	i
}
