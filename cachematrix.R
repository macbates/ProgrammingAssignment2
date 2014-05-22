##
## Create a 'special' matrix that allows calculation of the inverse to be
## cached. See the individual sub-functions for details.
##
makeCacheMatrix <- function(x = matrix()) {
  # First, set the cached value to NULL since this is a new matrix
  c.result <- NULL

  # Next, create sub-functions (makeCacheMatrix$whatever) that will allow getting the matrix,
  # solving the matrix, and modifying an existing matrix
  
  # Return a previously created matrix
  get <- function() return(x)
  
  # Solve an existing matrix
  inverse <- function () {
    # Check to see if it's cached and return it if so
    if (!is.null(c.result)) return (c.result)
    
    # Not cached, so solve the matrix, cache it, and return the solution
    c.result <<- solve(x)
    return(c.result)
    }
  
  # Modify an existing matrix by passing in a new one
  set <- function(y) {
    x <<- y # Reset the old matrix to the new one
    c.result <<- NULL # Modified matrix, so set cached inverse to NULL
    }
  
  # Now return the functions as a list that can be called
  list(get = get, set = set,
       inverse = inverse)  
}

##
## Solve a 'special' matrix that was created with the makeCacheMatrix function
##
cacheSolve <- function(x, ...) {
  # All that has to be done is to ask for the inverse. It will be caclulated
  # or returned from a cached value (if present).
  return(x$inverse())
}
