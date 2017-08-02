## The functions defined in this generate the matrix inverse from the calculations

## the makeCacheMatrix function would be returning the list of the following: 
## 1. matrix values will be set first
## 2. matrix values will be retreived
## 3. then we would be able to set the values of inverse matrix
## 4. finally we would be able to print the values of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## Incase the values are not calculated the values of the inverse matrix is calculated else it returns the calculated values

cacheSolve <- function(x, ...) {
        ## Inverse Matrix 'x' is returned
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
