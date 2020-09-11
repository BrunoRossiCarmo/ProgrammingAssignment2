#makeCacheMatrix Function:
        
        #This function will take the matrix and, furthermore, will get and
        #set the inverse matrix, returning a list of functions and sending 
        #the inverse matrix to cache.

makeCacheMatrix <- function(x = matrix()) {
        cache <- NULL
        set <- function(y){
                x <<- y
                cache <<- NULL
        }
        get <- function() x
        set_inv <- function(inverse) cache <<- inverse
        get_inv <- function() cache
        list(set=set,get=get,set_inv=set_inv,get_inv=get_inv)
}


makeCacheMatrix(matrix(1:9,3,3))

#cacheSolve Function:

        #This function will calculate if the inverse of the matrix created in the 
        #function above was cashed, returning the inverse matrix.

cacheSolve <- function(x, ...) {
        cache <- x$get_inv()
        if(!is.null(cache)) {
                message("getting cached data.")
                return(cache)
        }
        data <- x$get()
        cache <- solve(data)
        x$set_inv(cache)
        cache
}

#Matrix Created:
test <- matrix(sample(1:10,9),3,3)
print(test)
pass <- makeCacheMatrix(test)
cacheSolve(pass)

