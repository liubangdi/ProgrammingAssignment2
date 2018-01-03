## A pair of functions that cache the inverse of a matrix.

## This function creat a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()){
        inver <- NULL
        set <- function(y){
                x <<- y
                inver <<- NULL
        }
        get <- function ()x
        Set_Inverse <- function(solveMatrix) inver <<- solveMatrix
        Get_Inverse <- function() inver
        list(set = set, get = get, Set_Inverse = Set_Inverse, Get_Inverse = Get_Inverse)
}


cacheSolve <- function(x, ...) {
        inver <- x$Get_Inverse()
        if(!is.null(inver)){
                message("getting cached data")
                return(inver)
        }
        
        data <- x$get()
        inver <- solve(data)
        x$Set_Inverse(inver)
        inver
}
