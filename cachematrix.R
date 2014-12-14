## Dear classmates, excuse for my poor English
##
## makeCacheMatrix is passed an argument type of "matrix"
## and creates an "object" of type "list". 
## This "object" stores:
## 1) the value of the original matrix, which is passed as an argument
## 2) the value (initially set to "NULL") will be the value of cached inversed matrix
## 3) the list containing four functions, which:
##      set the value of variable containing the matrix
##          and assign NULL to variable containing the cached inversed matrix
##      get the value of variable containing the matrix
##      set the value of variable containing the inverse of the matrix
##      get the value of variable containing the inverse of the matrix

makeCacheMatrix <- function(Matrix = matrix()) {
    
        ## Inverse_Matrix is variable containing the cached inversed matrix
    
        Inverse_Matrix <- NULL
        
        ## function set(y) assigns the argument y (y is the matrix) to variable Matrix 
        ## and assigns NULL to variable Inverse_Matrix, 
        ## which will contain the cached inversed matrix
        set <- function(y) {
            Matrix <<- y
            Inverse_Matrix <<- NULL
        }
        ## function get() outputs variable Matrix containing the matrix
        get <- function() {
            Matrix 
        }
        ## function setinverse(y) stores the argument y (y will be the invesred matrix)
        ## to variable Inverse_Matrix
        setinverse <- function(y) {
            Inverse_Matrix <<- y
        }
        ## function getinverse() outputs variable Inverse_Matrix containing the invesred matrix
        getinverse <- function() {
            Inverse_Matrix
        }
        ## List of functions
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve computes the inverse of the matrix which stored in "object"  
## created by makeCacheMatrix 

cacheSolve <- function(Object_of_makeCacheMatrix, ...) {
    
    y <- Object_of_makeCacheMatrix$getinverse()
    ## In first call cacheSolve the variable y takes NULL, because y takes value of
    ## Inverse_Matrix, which has taked NULL in creating "object" by makeCacheMatrix
    
    ## Hence, in first call cacheSolve "if" is missing
    if(!is.null(y)) {
        ## This code does not work in first call cacheSolve 
        ## after creating "object" by makeCacheMatrix
        ## This code works in second and next call cacheSolve,
        ## it return non-null value y, assigning inverse of matrix, calculated earlier and
        ## stored in variable Invese_Matrix in "object"
        message("Getting cached inversed matrix")
        return(y)
        ## Function cacheSolve breaks
    }
    
    ## In first call cacheSolve the variable x takes value of variable Matrix
    ## which has been value as argument of makeCacheMatrix 
    ## in creating "object" by makeCacheMatrix
    x <- Object_of_makeCacheMatrix$get()
    
    ## In first call cacheSolve the variable y takes value of inverse of x (i.e. Matrix)
    z <- solve(x, ...)
    
    ## !!!!!!!!!!!!! Caching
    ## next line of code changes "object", created by makeCacheMatrix
    ## In first call cacheSolve the variable Inverse_Matrix inside "object" takes
    ## value of z (i.e. NULL is replaced with inverse of Matrix)
    ## It is possible because "<<-" inside "setinverse <- function(y) {Inverse_Matrix <<- y}
    Object_of_makeCacheMatrix$setinverse(z)

    ## It is simply output of z (i.e. inverse of Matrix)
    z

}
