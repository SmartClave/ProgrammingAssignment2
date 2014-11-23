## There will be two functions here to first of all create 
## a special “matrix” object that will cache its inverse 
## and a second one to compute the inverse of the matrix created.


## The first function “makeCacheMatrix” will create a matrix to cache its inverse

makeCacheMatrix <- function(x = matrix()) {   
 	i <- NULL 				## The function declares and initializes i to hold memory for 							## the inverse to be calculated 	
            set <- function(y) {		## This function sets the value of the Matrix 
                    x <<- y	  	
						## The special assignment operator updates the value of i
						## without allocating memory for for their values
                    i <<- NULL
            }
            get <- function() x			## This function gets the value of the Matrix
            setinverse <- function(inverse) i <<- inverse
            getinverse <- function() i
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)

}


## The second function “cacheSolve" computes the inverse of the matrix created above

cacheSolve <- function(x, ...) {
	i  <- x$getinverse() 		## This function checks if the inverse of the Matrix has been calculated
            if(!is.null(i)) {
                    message("getting cached data") ## Gets the inverse from the cache if calculated
                    return(i)
            }
            data <- x$get()
            i <- solve(data, ...) 	## The inverse is calculated if not already
            x$setinverse(i) 		## Sets the value of the inverse in the cache via the “set inverse”
            i				## Returns a matrix that is the inverse of 'x'
}
