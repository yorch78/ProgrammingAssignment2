## This piece of code are two functions that cache the inverse of a matrix

## makeCacheMatrix: creates a special object "matrix" class that cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
## Input parameter: x (by default creates an empty matrix object)
	
		## Inverse property initialization
        i <- NULL
        
        ## Setter method
        set <- function(y) {
        		## Assigns object y to the special matrix x
        		x <<- y
        		i <<- NULL
        }
        
        ## Getter method
        get <- function() {
        		## Returns the special matrix
        		x	
        } 
        
        ## Setter method
        setInverse <- function(inverse) {
        		## Assigns object inverse containing the inverse matrix to i
        		i <<- inverse
        }
        
        ## Getter method
        getInverse <- function() {
        		## Returns the inverse of the special matrix
        		i	
        } 
        
        ## Returns a list with the special matrix available methods
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve: calculates the inverse of the special "matrix" returned by makeCacheMatrix,
## just only when the matrix changes. If it's not changed, this funtion must return the
## inverse cached.

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
        i <- x$getInverse()
        
        ## If the inverse is set, return the cached inverse and finish
        if(!is.null(i)) {
        		message("getting cached data")
            return(i)
        }
        
        ## Get the original matrix to proceeed with the inverse calculation
        data <- x$get()
        
        ## Calculates the inverse matrix of data matrix
        i <- solve(data)
        
        ## Set the new inverse to the object
        x$setInverse(i)
        
        ## Returns the inverse calculated
        i
}


## Test cases execution
> miMatriz <-matrix(c(0,1,0,1,0,0,1,0,1),3,3)
> miMatriz
     [,1] [,2] [,3]
[1,]    0    1    1
[2,]    1    0    0
[3,]    0    0    1
> miM <-makeCacheMatrix(miMatriz)
> miM$get()
     [,1] [,2] [,3]
[1,]    0    1    1
[2,]    1    0    0
[3,]    0    0    1
> cacheSolve(miM)
     [,1] [,2] [,3]
[1,]    0    1    0
[2,]    1    0   -1
[3,]    0    0    1
> cacheSolve(miM)
getting cached data
     [,1] [,2] [,3]
[1,]    0    1    0
[2,]    1    0   -1
[3,]    0    0    1
> miM$get()%*%cacheSolve(miM)
getting cached data
     [,1] [,2] [,3]
[1,]    1    0    0
[2,]    0    1    0
[3,]    0    0    1