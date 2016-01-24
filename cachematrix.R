## The code represents 2 functions makeCacheMatrix and cacheSolve to create and access a matrix with pre-calculated inverse. 
## It is known that inverse matrix computation cost is high for large matrix. Caching matrix inverse can reduce the computation costs.
## Assumed that the matrix supplied is always invertible.

### makeCacheMatrix - is a function that creates a wrapper for an R matrix, such that its inverse matrix is additionally stored
### parameters: x - a matrix. 
### method call: w <- makeCacheMatrix(x), where w - is a makeCacheMatrix wrapper instance

makeCacheMatrix <- function(x = matrix()) {
###inv - internal variable to store inverse matrix.
	  inv <-NULL

###sets a matrix and its inverse
###parameters: y - square matrix
###method call: w$set(x), where w - is a makeCacheMatrix wrapper instance
	  set <- function(y) {
				x <<- y
				inv <<-NULL
        }
###gets a matrix
###parameters: none
###method call: w$get(), where w - is a makeCacheMatrix wrapper instance
        get <- function() x

###sets an inverse matrix	    
###parameters: inverse - a square matrix, that is (assumed to be) an inverse of a matrix
###method call: w$setinverse(x), where w - is a makeCacheMatrix wrapper instance
		setinverse <- function(inverse) inv <<- inverse

###gets an inverse matrix
###parameters: none
###method call: w$getinverse(), where w - is a makeCacheMatrix wrapper instance		
	    getinverse <- function() inv

###lists available methods for a matrix wrapper
###method call: w, where w - is a makeCacheMatrix wrapper instance		
        list(set = set, get = get,
		 setinverse = setinverse ,
		 getinverse = getinverse )
}

## cacheSolve - is a function that returns (pre-calculated) inverse of a given matrix
## parameters: x - a matrix, ... - all other arguments to calculate inverse, if any
## method call: inv_x <- cacheSolve(w), where w - is a makeCacheMatrix wrapper instance

cacheSolve <- function(x, ...) {
       inv <- x$getinverse()
###check if inverse matrix for x is already solved, return cached inverse
        if(!is.null(inv)) {
                message("getting cached matrix inverse")
                return(inv)
        }
###calculate inverse and return it
        data <- x$get()
  	   message("calculate matrix inverse")
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}