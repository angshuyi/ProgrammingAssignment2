# This function is used to create a special object that stores a numeric vector and caches its mean
# makeVector creates a special "vector", which is really a list containing a function to 
# set the value of the vector
# get the value of the vector
# set the value of the mean
# get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  # stores the cached value
  # initialize to NULL
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
  
  getinverse <- function() inv
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# This function assumes that the matrix is always invertible.
# The following function calculates the mean of the special "vector" created with the above function. 
# It first checks to see if the mean has already been calculated. 
# If so, it gets the mean from the cache and skips the computation. 
# Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function
cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  
  data <- x$get()
  
  inv <- solve(data)
  
  x$setinverse(inv)
  
  inv
}

# Test Run 1
#> x = rbind(c(1, -1/4), c(-1/4, 1))
#> m = makeCacheMatrix(x)
#> m$get()

# Test Run 1 - Result 
# [,1]  [,2]
# [1,]  1.00 -0.25
# [2,] -0.25  1.00

# Test Run 2
# > cacheSolve(m)

# Test Run 2 - Result
# [,1]      [,2]
# [1,] 1.0666667 0.2666667
# [2,] 0.2666667 1.0666667