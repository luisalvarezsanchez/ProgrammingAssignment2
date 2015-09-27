
## Short comment: makeCacheMatrix is a function that creates 
## a special "matrix" object which is really a list containing 
## a function to set (or get)  the value of matrix m, or 
## a function to set (or get)  the value of the inverse of m

makeCacheMatrix <- function(m = matrix()) {
        inv <- NULL  
        set <- function(y) {
                m <<- y
                inv <<- NULL
        }
        get <- function() m        
        ## gets the value of m matrix
        setinverse <- function(inverse) inv <<- inverse 
        ## inverse argument MUST be the real inverse
        getinverse <- function() inv 
        ## returns NULL or either the real value of inverse matrix
        ## provided by the user calling the function
        list(set = set, get = get,
            setinverse = setinverse,   
            getinverse = getinverse)
}


## Short comment: cacheSolve is a function that computes the 
## inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'm'
        inv <- m$getinverse()
        if(!is.null(inv)) {
              message("getting cached data")
              return(inv)
        }
        ## if the inverse has already been calculated we get cached data
        ## otherwise, we get the matrix with function m$get, then
        ## we calculate the real inverse with solve function and
        ## we set the real inverse with m$setinverse function
        data <- m$get()
        inv <- solve(data, ...)
        m$setinverse(inv)
        inv
}