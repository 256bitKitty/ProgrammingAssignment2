## Two functions that will speed up calculations for large data sets using cache (based on lexical scoping)
## 	makeCacheMatrix function will cache the inverse of a matrix
## 	cacheSolve function will calculate the inverse of a matrix using the solve built-in function
## Based on the example given in the assignment (makeVector function)

## Caching the Inverse of a Matrix, this function does the following:
## sets the value of the matrix
## gets the value of the matrix
## sets the value of the inverse
## gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## If there is no cached matrix (m) then 
## this function calls the built-in function 
## named 'solve' to calculate the inverse of the matrix (data)

cacheSolve <- function(x, ...) {
                m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
