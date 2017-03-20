## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# The first function, makeCacheMatrix creates a special "matrix", which is 
#really a list containing a function to

# set the value of the Matrix
# get the value of the Matrix
# set the value of the inverse
# get the value of the inserve

makeCacheMatrix <- function(x = matrix()){
        invers <- NULL
        set <-  function(y){
                invers <<- NULL
                x <<- y
        } 
        get <- function() x
        getinverse <- function() invers
        setinverse <- function(inve) invers <<- inve 
        list(set = set, get = get, 
             getinverse = getinverse,
             setinverse = setinverse)
}


## Write a short comment describing this function
# The following function calculates the inverse of the special "matrix" created
# with the above function. However, it first checks to see if the inverse
# has already been calculated. If so, it gets the inverse from the cache and 
# skips the computation. Otherwise, it calculates the inverse of the data 
# and sets the value of the inverse in the cache via the setmean function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inversedMat <- x$getinverse()
        
        if (!is.null(inversedMat)) {
                message("getting cached data !")
                return(inversedMat)
        }
        message("cached data not found")
        data <- x$get()
        inversedMat <-  solve(data, ...)
        x$setinverse(inversedMat)
        inversedMat
}

##Somes tests
z = matrix(data = c(1,2,3,4), nrow = 2, ncol=2)
z2 <- makeCacheMatrix(z)
cacheSolve(z2) #should message "cached data not found"
cacheSolve(z2) #should message "getting cached data"
