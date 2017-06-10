## Week 3 Assignment for R Programming for JHU
## B. Kavalar    10 June 2017

##loading MASS library to use ginv(). solve() limits matrix size.
##assignment only suggested the use of solve() but not required.
library(MASS)

##creating function for makeCacheMatrix based on requirements
#for passing a matrix and creating function prototypes

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
      }

#Create function prototypes for a special matrix which
#is a list containing a functions to:
#  1.  set the value of the matrix
#  2.  get the value of the matrix
#  3.  set the value of the inverse
#  4.  get the value of the inverse
  
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#create cacheSolve function to get results from 
#makeCacheMatrix function and create inverse matrix

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  
  #using alternative matrix inversion function ginv()
  m <- ginv(data, ...)
  x$setinverse(m)
  m
}

#Test condition #1
test_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
test_matrix$get()
cacheSolve(test_matrix)

#Test condition #2
test_matrix <- makeCacheMatrix(matrix(c(2, 3, 1, 4), nrow=2, ncol=2))
test_matrix$get()
cacheSolve(test_matrix)

#Test condition #3
test_matrix <- makeCacheMatrix(matrix(1:6, 3, 3))
test_matrix$get()
cacheSolve(test_matrix)

#Test condition #4
test_matrix <- makeCacheMatrix(matrix(1:25, 5, 5))
test_matrix$get()
cacheSolve(test_matrix)

