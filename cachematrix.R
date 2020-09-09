## Comment on overall behaviour of functions:
## 
## The two functions work in conjunction.
## 
## "makeCacheMatrix stores 'x' in the form of a matrix and makes 'x' available
## outside of the function's environment.
## 
## It will be used to store both 'x' and its inverse.
## 
## 
## "cacheSolve" will return the inverse of 'x', first by checking to see if the
## inverse is already stored in 'makeCacheMatrix' and returning it from there.
## 
## If it is not stored in 'makeCacheMatrix', it will directly solve for 'x' and
## both store the answer in 'makeCacheMatrix' and print the answer to console.
## 
## A better look at my current 
## understanding can be found by referring to my thinking on the original
## functions in the comments at the end of the script.




## Short comment about function 'makeCacheMatrix':
## This function creates a special "matrix" object that can cache its inverse.
## 
## This means it will store both the matrix and its inverse which will be used
## by 'cacheSolve'

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Short comment about function 'cacheSolve':
## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix.
## 
## This means it will take 'x' from 'makeCacheMatrix' and calculate its inverse
## by either returning the stored inverse in 'makeCacheMatrix', or calculating
## the inverse of 'x' if no stored inverse is found.

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}












##  ## Original functions from Assignment with my understanding of them
##  
##  ## From my understanding, this function takes 'x', a numeric vector of
##  ## arbitrary length and stores it while keeping track of it and 'm'.
##  ## e.g. "term <- makeVector(1:10)" stores 1:10 as x and resets 'm' to NULL
##  
##  ## With x set initially, we can now use the four functions nested in this one.
##  ## The functions are 'set', 'get', 'setmean', 'getmean'.
##  
##  ## 'set' inputs a new 'x' value and resets 'm' to NULL
##  ## e.g. "term$set(3:5)" will change 'x' from 1:10 to 3:5 and return m to NULL
##  
##  ## 'get' returns the current value of 'x'
##  ## e.g. "term$get()" will return 'x' as 3:5
##  
##  ## 'setmean' inputs a new value for 'm'
##  ## e.g. term$setmean(5) will change 'm' from NULL to 5
##  ##
##  ## 'getmean' returns the current value of 'm'
##  ## e.g. term$getmean() will return 'm' as 5
##
##  makeVector <- function(x = numeric()) {
##  m <- NULL
##  set <- function(y) {
##    x <<- y
##    m <<- NULL
##  }
##  get <- function() x
##  setmean <- function(mean) m <<- mean
##  getmean <- function() m
##  list(set = set, get = get,
##       setmean = setmean,
##       getmean = getmean)
##  }
##  
##  ## This function is an if function which takes advantage of "makeVector" terms
##  ## being accessible from outside the function.
##  ## 
##  ## This function makes use of the previous function 'makeVector' and needs
##  ## '$getmean()', '$setmean' and '$get' to be defined by it.
##  ## This means it won't accept a normal numeric vector, but only the result of
##  ## 'makeVector'.
##  ##
##  ## e.g. "cachemean(term)" will return 5. Remember I inserted 5 manually from before
##  ##
##  ## e.g. "term$set(3:5)" this will reset 'm' to NULL and leave 'x' as 3:5
##  ##
##  ## e.g. "cachemean(term)" will now return 4. notice it didn't print "getting cached data"
##  ##
##  ## e.g. running "cachemean(term)" will return 4, with the message, "getting cached data"
##  
##  cachemean <- function(x, ...) {
##    m <- x$getmean()
##    if(!is.null(m)) {
##      message("getting cached data")
##      return(m)
##    }
##    data <- x$get()
##    m <- mean(data, ...)
##    x$setmean(m)
##    m
##  }