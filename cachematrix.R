# Function 1 (makeCacheMatrix) --> create a matrix that can cache its inverse.
# Function 2 (cacheSolve)      --> create the inverse of makeCacheMatrix (in function 1)

# makeCacheMatrix:

# the input is a matrix 'x'
# 'm' will be the matrix and when makeVector is called, m will be set to NULL.
# 'get' returns the value of the matrix
# 'setmatrix' will store the matrix
# 'getmatrix' will return the matrix
# 'list' is a list of internal functions so a calling function knows how to access those methods

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL       
    get <- function() { x }  
    
    setmatrix <- function(matrix)  { m <<- matrix }
    
    getmatrix <- function() { m } 
    
    list(get = get, 
         setmatrix = setmatrix,
         getmatrix = getmatrix) 
}

# cacheSolve:

# the input is 'x', which was created with makeCacheMatrix
# 'm' will get the value of the matrix within x
# if the matrix is not NULL, print "getting cached data" and return the matrix.

# if the matrix was NULL the inverse matrix is returned 
# store the inverse matrix in x with 'x$setmatrix(m)'

# 'm' prints a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)
    m
}
