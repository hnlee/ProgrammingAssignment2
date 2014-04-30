## These functions eliminate redundant calculations by allowing the user
## to cache the inverse of a matrix after first solving for it
## and to retrieve that inverse when needed without having to solve
## for it again.

## This function takes a matrix as input and returns a list of functions
## that allow the user to cache the values of the input matrix and its inverse
## and to retrieve those same values from cache.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    # Define function to cache the input matrix
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    # Define function to retrieve the input matrix from cache
    get <- function() {
        x
    }
    
    # Define function to cache the inverted matrix
    setinverse <- function(invertedmatrix) {
        inverse <<- invertedmatrix
    }
    
    # Define function to retrieve the inverted matrix from cache
    getinverse <- function() {
        inverse
    }
    
    # Return a list of the above functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function takes the list output of makeCacheMatrix() as input
## and checks to see if the inverse of the matrix has already been solved.
## If it has, then it retrieves that value. If not, it solves for the inverse
## and stores the value in cache.

cacheSolve <- function(x, ...) {
    # Retrieve cached inverse
    inverse <- x$getinverse()
    
    if(!is.null(inverse)){
        # If inverse has already been calculated, return its value from cache
        message('getting cached data')
        return(inverse)
    } else {
        # Otherwise, calculate inverse and cache it
        matrix <- x$get()
        inverse <- solve(matrix, ...)
        x$setinverse(inverse)
        return(inverse)
    }
}
