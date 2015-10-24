### makeCacheMatrix is a function that accepts a matrix (variable 'x') as its input.
### makeCacheMatrix has fours subfunctions: set, get, setinverse, getinverse.

## List of subfunctions for main function "makeCacheMatrix".
  # The "set" subfunction allows us to change the matrix stored in the main function "makeCacheMatrix".
  # The "get" function returns the matrix 'x' stored in the main function. 
  # The "setinverse" function allows us to input the inverse matrix (as the variable 'i') of the main matrix 'x' stored in the main function "makeCacheMatrix".
  # The "getinverse" function returns the stored inverse matrix in the "setinverse" function.

makeCacheMatrix <- function(x = matrix()) {  #Defines the main input of "makeCacheMatrix" to be a Matrix.
    i <- NULL # Makes the inverse matrix value to be 'Blank'.
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


### The cacheSolve function returns the inverse of the matrix inputed in the "makeCacheMatrix" function.
### 

cacheSolve <- function(x, ...) { # The input is the object used to store the "makeCacheMatrix" function and its associated main matrix.
          i <- x$getinverse() # Retreives stored inverse matrix from the "makeCacheMatrix" function.
          if(!is.null(i)) {  # Logical statement to check if to see there was an inputed inverse matrix from the "makeCacheMatrix" function.
                    message("getting cached data") # Message indicating that the cacheSolve function is retrieving the inputed inverse matrix from the "makeCacheMatrix" function
                    return(i) # Returns stored inverse matrix.
          }
          data <- x$get () # Retrieves stored main matrix from "makeCacheMatrix" function.
          i <- solve(data, ...) # Computes inverse matrix from the stored main matrix from the "makeCacheMatrix" function.
          x$setinverse(i) # Stores the computed inverse matrix in the "makeCacheMatrix" function.
          i # Returns the inverse matrix.
      }
        ## Return a matrix that is the inverse of 'x'

