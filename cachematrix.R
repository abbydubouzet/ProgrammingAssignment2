## The 2 functions lessens the computation cost from matrix inversion

## Inverses cache through the use of matrix
makeCacheMatrix <- function(mtrxOriginal = matrix()) {
        # Set NULL value to mtrxInverse
        mtrxInverse <- NULL
        
        set <- function(y) {
          # Makes mrtxOriginal equal to input value
          mtrxOriginal <<- y
          # Clears value cached by cacheSolve
          mtrxInverse <<- NULL
        }
        
        # Gets value of mtrxOriginal
        get <- function() mtrxOriginal
        # Sets the inverse of mtrxInverse
        setInverse <- function(inverse) mtrxInverse <<- inverse
        # Gets the inverse of mtrxInverse
        getInverse <- function() mtrxInverse
        # Creates an object through a returned list
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Evaluates the inverse of makeCacheMatrix's returned value
cacheSolve <- function(mtrxOriginal, ...) {
        # Calls getInverse function from makeCacheMatrix
        mtrxInverse <- mtrxOriginal$getInverse()
        
        # Checks if object mtrxInverse is not NULL
        if(!is.null(mtrxInverse)) {
          # If not then a message and the inverse will be returned to the parent environment
          message("getting cached data")
          return(mtrxInverse)
        }
        
        # If the if-statement is false, the input object will be made an inverse
        data <- mtrxOriginal$get()
        mtrxInverse <- solve(data, ...)
        mtrxOriginal$setInverse(mtrxInverse)
        # Also, the object would be outputted
        mtrxInverse
}
