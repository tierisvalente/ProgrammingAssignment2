## Below are two functions that are used to create a special
## object that stores a matrix and cache's its inverse

## This function creates list containing functions to
## set/get a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(inverse) i <<- inverse
    
    getinverse <- function() i
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function return a cached matrix that is the inverse of 'x'
## If the inverse is not cached, this function will calculate it
## and store the data
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    
    data <- x$get()
    
    i <- solve(data)
    
    x$setinverse(i)
    
    i
}
