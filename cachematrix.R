## cachematrix.R 
##Includes 2 functions : **makeCacheMatrix()** and **cacheSolve()** to cache and calculate the inverse of given matrix.

## makeCacheMatrix()  
##Function **makeCacheMatrix()** create a list to

##1. set the value of the matrix via function *set()*
##2. get the value of the matrix via function *get()*
##3. set the value of the inverse matrix via function *setInverse()*
##4. get the value of the inverse matrix via function *getInverse()*
        

makeCacheMatrix <- function(x = matrix()) {
                m <- NULL
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
                get <- function() x
                setInverse <- function(inverse) m <<- inverse
                getInverse <- function() m
                list(set = set, get = get,
                     setInverse = setInverse,getInverse = getInverse)
}


##Function cacheSolve()

##Function **cacheSolve()** calculates the inverse of the matrix passed by **makeCacheMatrix()**.

##Checks if the inverse has already been calculated.

##1. If calculated, 
##*  it gets the inverse from the cache 
##*  skips the computation.   

##2. Otherwise, 
##*  it calculates the inverse of the matrix 
##*  sets the value of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}

