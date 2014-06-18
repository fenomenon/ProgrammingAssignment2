## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Creates a special "matrix", which is a list containing functions to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of the matrix
# 4. get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL  # initialize mean i to NULL
  
  # assigns the matrix to x of the parent environment
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x # gets the matrix stored in x
  setinverse <- function(inverse) i <<- inverse  # assigns the inverse to m of parents environment
  getinverse <- function() i   # gets the inverse stored in i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function
# returns a matrix that is the inverse of 'x'
# if the matrix hasn't been previously computed, then it will compute it
# otherwise, it is obtained from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
