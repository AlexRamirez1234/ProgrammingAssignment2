## Functions that cache the inverse of a matrix

## makeCacheMatrix: Creates a list object with functions, that cache the matrix  
## and its inverse

makeCacheMatrix <- function(x = matrix()) {
    # Assign a NULL value to the inverse
    i <- NULL
    
    # Function to set the matrix
    set <- function(y){
        x <<- y
        i <-- NULL
    }
    
    # Function to get the matrix
    get <- function(){
        x
    }
    
    # Function to set the inverse of the matrix
    setinverse <- function(inverse){
        i <<- inverse
    }
    
    # Function to get the inverse of the matrix
    getinverse <- function(){
        i
    }
    
    # Return a list of the functions
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## CacheSolve: calculate the inverse of the matrix cached by makeCacheMatrix,
## in case the inverse was already calculate, just retrieve the value of the 
## inverse that was cached.

cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    
    # Just return the inverse if its already cached
    if (!is.null(i)){
        message("getting inverse cached data")
        return(i)
    }
    
    # get matrix from our object
    data <- x$get()
    
    # Calculate the inverse
    i <- solve(data, ...)
    
    # Set inverse in the object
    x$setinverse(i)
  
    i  
}

#example
example <- makeCacheMatrix(matrix(c(3,2,5,2,3,2,5,2,4), 
                                  nrow = 3, ncol = 3)) #Cache matrix
example$get() #Retrieve the matrix
cacheSolve(example) #Calculate the inverse of the matrix
cacheSolve(example) #Retrieve the inverse now that it has been cached
example$getinverse() #Verify that the inverse was cached

# To prove another example, just write example$set("desired matrix")
