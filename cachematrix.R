## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
######################################################################
##																	
## makeCacheMatrix creates creates a special "vector", which is 	
## really a list containing four functions:  						
##																	
## 1. set the value of the matrix (set)								
## 2. get the value of the matrix (get)								
## 3. set the value of the inverse (setinverse)						
## 4. get the value of the inverse (getinverse)						
##																
######################################################################

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL

	## 1. set the value of the matrix (set)	
	set <- function(y) {
        x <<- y
		inv <<- NULL 
	}
	
	## 2. get the value of the matrix (get)	
	get <- function() x
	
	## 3. set the value of the inverse (setinverse)	
	setinverse <- function(inverse) inv <<- inverse
	
	## 4. get the value of the inverse (getinverse)
	getinverse <- function() inv
	
	## create the special "vector" containing list of functions
	list(set = set, get = get,
     	setinverse = setinverse,
     	getinverse = getinverse)
}


## Write a short comment describing this function
##############################################################################
##																			
## cacheSolve calculates the inverse of the special "vector" created		 
## with the above function. However, it first checks to see if the 			
## inverse has already been calculated. If so, it gets the inverse from 
## the cache and skips the computation. Otherwise, it calculates the 	
## inverse of the data and sets the value of the inverse in the cache 		
## via the setinverse function.											
##																		
##############################################################################

cacheSolve <- function(x, ...) {

	## Get the inverse
	inv <- x$getinverse()
        
	## Check if inv is not null i.e. inverse is present in cache
	## if yes, print a message, return the inv and exit function
	if(!is.null(inv)) {
        message("getting cached data")
		return(inv) 
	}
	
	## inv is null, therefore, we need to calculate the inverse
	
	## get the matrix and store in data
	data <- x$get()
	
	## Use solve function to calculate inverse and store in inv
	inv <- solve(data, ...)
	
	## Use setinverse function to store the inverse for future use
	x$setinverse(inv)
	
	## return the inverse
	inv
}
