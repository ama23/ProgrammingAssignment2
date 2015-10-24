## makeCacheMatrix is a function that accepts a matrix (variable 'x') as its input.
## makeCacheMatrix has fours subfunctions: set, get, setinverse, getinverse.
  # The "set" subfunction allows us to change the matrix stored in the main function "makeCacheMatrix".
  # The "get" function returns the matrix 'x' stored in the main function. 
  # The "setinverse" function allows us to input the inverse matrix (as the variable 'i') of the main matrix 'x' stored in the main function "makeCacheMatrix".
  # The "getinverse" function returns the stored inverse matrix in the "setinverse" function.

makeCacheMatrix <- function(x = matrix()) { #Defines the main input of "makeCacheMatrix" to be a Matrix.
    i <- NULL # Sets the inverse matrix value to be 'Blank'
    set <- function(y) { # set function allows us to change the matrix of the main function.
        x <<- y # Swaps matrix "x or the original main matrix" for the "new" main matrix or "y". The double arrows is needed to ensure that the the new value "y" is avaiable to all of the set functions.
        i <<- NULL # if the matrix of the main function is changed, we need to "reset" the inverse matrix.
    }
    get <- function() x # Returns the matrix stored in the main 
    setinverse <- function(solve) i <<- solve # Sets the inputed inverse matrix to the value "i".
    getinverse <- function () i # Returns the inputed inverse matrix value.
    list(set = set, get = get,# The list function allows us to the store the four subfunctions (set, get, setinverse, and getinverse) in the main "makeCacheMatrix" function.
         setinverse = setinverse,
         getinverse = getinverse)
# When we assign makeCacheMatrix to an object, the object should have all four subfunctions as well. 
}


## Write a short comment de

cacheSolve <- function(x, ...) {
          i <- x$getinverse()
          if(!is.null(i)) { 
                    message("getting cached data")
                    return(i)
          }
          data <- x$get ()
          i <- solve(data, ...)
          x$setinverse(i)
          i
      }
        ## Return a matrix that is the inverse of 'x'

