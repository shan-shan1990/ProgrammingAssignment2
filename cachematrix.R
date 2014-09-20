# makeCacheMatrix Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(B = matrix(1:60, ncol=5, byrow = FALSE, dimnames= NULL)) 
             {
          as.matrix(B,...)
# The object does not calculate the inverse, just saves it inside.
# Saves the matrix to variable B and its inverse to variable m in scope.
          m <- NULL
  set <- function(B) {# set: sets matrix and resets cached inverse
    B <<- y
    m <<- NULL
  }
  get <- function(B) {# get: returns matrix
    B
  }
  setsolve <- function(solve) {# setSolve: saves solve value
    m <<- solve
  }
  getsolve <- function(solve){# getSolve: returns cached inverse value
    m
  } 
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}
# Function to get the inversed matrix from a special object created by makeCacheMatrix.
# Takes the object of that type as an argument 'x', checks if the inverse value is already
# cached, and if it is returns the cached value; if not, this function calculates the
# inverse for the matrix saved in the 'x', saves it into 'x' cache using method 'setSolve'
# and returns the result.
cachesolve <- function(B = matrix(1:60, ncol=5, byrow = FALSE, dimnames= NULL), ...) {
  m <- B$getsolve(B)
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- B$get(m)
  m <- solve(data, ...)
  B$setsolve(m)
  m
}
