##  Below are two functions that are used to create a special object that stores
##  a matrix and caches its inverse

## The first function, makeCacheMatrix creates a list containing 
## the functions to:

# 1. set the matrix
# 2. get the matrix
# 3. set the inverse of the matrix
# 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function() x
    setinverse<-function(solve) m<<- solve
    getinverse<-function() m
    list(set=set, get=get,
         setinverse=setinverse,
         getinverse=getinverse)
}


## The following function creates the iverse of the matrix created with the 
## above function. However, it first checks to see if the inverse has already
## been created. If so, it gets it from the cache and skips the computation. 
## Otherwise, it produces the inverse of the matrix and saves it in the cache
## via the setmean function.

cacheSolve <- function(x=matrix(), ...) {
    m<-x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setinverse(m)
    m
}

## Testing

a<-makeCacheMatrix()# Assign the list to 'a'
a$set(matrix(1:4,2,2))# Generate a matrix
cacheSolve(a)
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
cacheSolve(a)
# getting cached data
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

