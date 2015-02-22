# Matrix inversion is usually a costly computation.  These routines are designed
# to cache the computation result and return it if it has already been calculated

# makeCacheMatrix will store the matrix, calculate its inverse on demand and if already calculated
# return the previously calculated result.
makeCacheMatrix <- function(x = matrix()) {
  m_inverse <- NULL
  
  # reassign value
  set <- function(y) {
    x <<- y
    m_inverse <<- NULL
  }
  get <- function() x   # returns value of interest before function applied
  set_inverse <- function(inverse) m_inverse <<- inverse  # saves result
  get_inverse <- function() m_inverse                     # returns result or NULL if yet to calc
  list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## Write a short comment describing this function
# function designed to either returned a cached value of a matrix inverse or calculate the value and return that.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m_inverse <- x$get_inverse()
  
  if(!is.null(m_inverse)) {
    message("getting cached data.")
    return(m_inverse)
  }
  data_matrix <- x$get()
  m_inverse <- solve(data_matrix)
  x$set_inverse(m_inverse)
  m_inverse
}
# some tests.....
# easy sample matrix
# > mat<- matrix(1:4,2,2)
# > mat
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > solve(mat)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > 
# > my_cache <-makeCacheMatrix(mat)
# > my_cache$get()
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > my_cache$get_inverse()
# NULL
# > cacheSolve(my_cache)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(my_cache)
# getting cached data.
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > 
