## makeCacheMatrix takes a matrix and returns a list
## which contains information that can be retrieved later
## i.e. which is persistent. the list has four objects
## which are essentially functions to the persistent data
## 1. function that stores the matrix 
## 2. function that retrieves the stored matrix
## 3. function that stores the value of the inverse of an invertible
## matrix
## 4. function that returns the stored inverse of the stored matrix OR
## NULL
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	setmatrix <- function(y){
		  x <<- y
		  mean <<- NULL
	}
	getmatrix <- function() x
	setinverse <- function(inversematrix) inverse <<-inversematrix
	getinverse <- function() inverse
	list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse,
	getinverse = getinverse) 
}


## cacheSolve function takes a list (in the format returned by
## makeCacheMatrix) and returns the inverse of the stored matrix.
## If the stored matrix's inverse was not computed before, this
## function computes it, stores it inside the list's objects
## and then returns the inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inverse <- x$getinverse()
	if(!is.null(inverse)){
		message("getting cached inverse matrix")
		return(inverse)
	}
	squarematrix <- x$getmatrix()
	inverse <- solve(squarematrix, ...)
	x$setinverse(inverse)
	inverse
}
