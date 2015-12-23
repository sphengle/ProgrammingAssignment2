## The makeCacheMatrix() function is applied to a matrix in order to create a list
## as a kind of wrapper object.
## The second function, cacheSolve, takes this wrapper object and computes and caches
## the inverse of the matrix.  Subsequent calls to cacheSolve() will return the cached
## version of the matrix inverse.



## A function to make a cached wrapper version of a matrix. Returns a list containing
## function pointers.
makeCacheMatrix <- function(x = matrix()) {
  
  # initialise inverse cached matrix to null
  inv <- NULL
  
  # for setting a new matrix, must clear the cached value
  set <- function(y) 
  {
    x <<- y
    inv <<- NULL
  }
  
  # getting the current matrix
  get <- function()
  {
    x
  }
  
  setinverse <- function(inverse)
  {
    # set the cached value
    inv <<- inverse
  }
  
  getinverse <- function() 
  {
    # simply return the value
    inv
  }
  
  # return the wrapper object
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



# function to get inverse matrix
cacheSolve <- function(x, ...) 
{
  # get cached value
  inv <- x$getinverse()
  
  if(!is.null(inv)) 
  {
    # if we have a cached version then do message and return now
    message("getting cached data")
    return(inv)
  }
  
  # get original matrix
  sourceMatrix <- x$get()
  
  # invert it
  inverseMatrix <- solve(sourceMatrix, ...)
  
  # save the inverted matrix in the cache
  x$setinverse(inverseMatrix)
  
  # return inverse
  inverseMatrix
}

# ------ TESTING  --------

# create a 3 x 3 matrix
m <- matrix( rnorm(9), 3, 3)

# look at it
m

# solve it
solve(m)

# make the special matrix wrapper
mc <-makeCacheMatrix(m)

# check the get() function and compare to original
mc$get()

# now apply cacheSolve() a couple of times - 2nd one should show a message
cacheSolve( mc )
cacheSolve( mc )

