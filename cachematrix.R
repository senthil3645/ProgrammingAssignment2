#Programming Assignment 2: Lexical Scoping

#Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. 

## Emulating the **Sample code snippet provided** in the Assignment instructions which
##caches the mean of a vector, the difference here is a matrix is being passed to 
##compute the inversion instead of a numeric vector to compute the mean

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
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


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, the results from cache are retrieved.
# If not, the program computes the inverse, sets the value in the cache through
# setinverse function.

#The inverse of a 2X2 matrix, is calculated using this formula:
#Example: Matrix A  
#http://www.mathwords.com/i/inverse_of_a_matrix.htm
#       [,1] [,2]
#[1,]    a    b
#[2,]    c    d
#A Inverse = 1/det A  * | d   -c | === 1/(ad - bc) *| d   -c | 
#                       | -b   a |                  | -b   a |

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Retrieving from Cache.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

## Test Results
## > x = rbind(c(1, 2), c(3, 4))
## > m = makeCacheMatrix(x)
## > m$get()
##       [,1] [,2]
##[1,]    1    2
##[2,]    3    4

## Initial run, No cahe avaialble
## > cacheSolve(m)
##[,1] [,2]
##[1,] -2.0  1.0
##[2,]  1.5 -0.5

## For the Later Run , Inverse matrix is loaded from cache
## > cacheSolve(m)
## getting cached data.
##Retrieving from Cache.
##    [,1] [,2]
##[1,] -2.0  1.0
##[2,]  1.5 -0.5