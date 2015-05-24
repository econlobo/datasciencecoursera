##Part I
## Notes: This function creates a list, called a "vector"
## The function sets and gets the values of the "vector"
## The function also set and gets the values of the vector's mean
makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


##Part II
## Notes: This function calculates the mean of the "vector" from Part I
## To do so, it first checks if the mean has already been created.
## If the mean has been created, then it skips the computation.
## Otherwise, it calculates the value of the mean and sets the value in the cache.
cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

## Part III
## **Assignment #3**
## The purpose of the following functions is to accomplish the following:
## 1. Convey a lesson in Lexical Scoping (where does an operation look first?)
## 2. Convey a lesson about the speed of caching vs. computation
## The functions create lists that act as pseudo-objects
## The functions also find the means of these "vectors" and the inverse of the matrix.
## Finally, the functions utilize the cache if theses calculations have been performed.


## Notes: This function creates a "matrix".
## The inverse of this matrix can be cached
## This function sets and gets the values of the matrix and caches them in the list
makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(z) {
    x <<- z
  }
  get <- function() x
  setinverse<- function(inverse) inv_x <<-inverse
  getinverse <- function() inv_x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Part IV
## Notes: This function computes the inverse of the "matrix" from "Part III
## If the inverse was already calculated, then this function retrieves it from the cache. 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_x <- x$getinverse()
  if (!is.null(inv_x)) {
    message("Retrieving. Wait One.")
    return(inv_x)
  } else {
    inv_x <- solve(x$get())
    x$setinverse(inv_x)
    return(inv_x)
  }
}
