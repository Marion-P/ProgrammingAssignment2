## The function makeCacheMatrix creates a special "matrix" object that can 
##cache its inverse. cacheSolve returns the inverse of a matrix, first by 
##whether the result is available in the cache, then if not by calculating
##it.

## makeCacheMatrix calculates the inverse of a matrix and stores it in a 
## matrix object. This allows to reduce computation time for long
#calculations

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL #initialises an empty matrix object to cache the inverse
  set <- function(y) { #allows to define the value of the matrix
    x <<- y #use <<- to ensure the modifications affect the parent environment
    inverse <<- NULL #use <<- to ensure the modifications affect the parent environment
  }
  get <- function() x #returns the matrix x
  setinverse <- function(inverseMatrix) inverse <<- inverseMatrix #sets the inverse of the matrix
  getinverse <- function() inverse #returns the inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) #returns a list with the four previously described functions

}


## cacheSolve returns the inverse of a matrix. It starts by looking in 
#the parent environment if the inverse exists. If not, it calculate it 
#and stores it in the cache matrix for future use

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse() #assigns to inverse the value in the cache matrix
  if(!is.null(inverse)) {  #checks if the inverse value has ben calculated and stored before
    message("getting cached data") #if yes , inform the user
    return(inverse) #and return the inverse
  }# if not:
  data <- x$get() #assign to a temporary/local variable data the value of the matrix x
  inverse <- solve(data)#calculate the inverse 
  x$setinverse(inverse) #store the inverse in the cache
  inverse #return the inverse
}
