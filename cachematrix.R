## Put comments here that give an overall description of what your
## functions do
## The program takes a square matrix and returns an inverse of the input square matrix.
## The program does not check if the input square matrix is exactly singular.
## Write a short comment describing this function

##Sample Test Case##
#mat1 <- makeCacheMatrix(c(1,0,1,1),2,2)
#cacheSolve(mat1)
##output##
#       [,1] [,2]
# [1,]    1   -1
# [2,]    0    1
#
#cacheSolve(mat1)
##output##
#getting cached data
#       [,1] [,2]
# [1,]    1   -1
# [2,]    0    1

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <- NULL
    }
    get <- function() x
    setinverse <- function(invmatrix) m <<- invmatrix
    getinverse <- function() m
    list( set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## Write a short comment describing this function
##The function below checks if an inverse of input matrix already exists. 
##If it exists then it returns the inverse along with the message "getting cached data" 
##else it computes the inverse afresh, stores and returns the same

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m<-x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
##More test case
# > source("cachematrix.R")
# > d1<-matrix(c(4,1,7,2,7,3,8,4,5),3,3)
# > d1
# [,1] [,2] [,3]
# [1,]    4    2    8
# [2,]    1    7    4
# [3,]    7    3    5
# > cm<-makeCacheMatrix(d1)
# > cacheSolve(cm)
# [,1]         [,2]        [,3]
# [1,] -0.1 -0.060869565  0.20869565
# [2,] -0.1  0.156521739  0.03478261
# [3,]  0.2 -0.008695652 -0.11304348
# > cacheSolve(cm)
# getting cached data
# [,1]         [,2]        [,3]
# [1,] -0.1 -0.060869565  0.20869565
# [2,] -0.1  0.156521739  0.03478261
# [3,]  0.2 -0.008695652 -0.11304348
