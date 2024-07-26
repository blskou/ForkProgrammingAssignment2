## The aime of the functions are to compute and cache the inverse of a matrix.

## makeCacheMatrix is a function to make a special matrix that can cache 
## the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL                             #Setting the inverse matrix to NULL 
    set <- function(y){                     #Set the value of the matrix
        x <<- y
        inv <<- NULL
    }
    get <- function() x                     #Get the value of the matrix
    setinv <- function(solve) inv <<- solve #Set the value of the inverse matrix
    getinv <- function() inv                #Get the value of the inverse matrix
    list(set = set, get = get, 
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve is a function that computes the inverse of the matrix returned 
## from the function makeCacheMatrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()           #Check to see if inverse has been computed            
    if(!is.null(inv)) {         #If inverse has been computed gets cached inverse
        message("getting cached data")
        return(inv)
    }                           
    data <- x$get()
    inv <- solve(data, ...)     #Calculating the inverse based on data
    x$setinv(inv)               #Sets inverse of the data in the cache
    inv
}
