## This function creates a special type of matrix made of a list containing four functions:
## 1. set - which sets the value of the matrix
## 2. get - which gets the value of the matrix
## 3. setinv - which sets the value of the inverse of the matrix
## 4. getinv - which gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix())
{
## set inv to NULL the first time the function is called as the inverse hasn't been calculated yet  
  cacheinv <- NULL
  set <- function(y)
  {
    ## assign new value to x in the function environment
    x <<- y
    ## reset cache value to NULL as there was a change in value. One improvement could be to verify if
    ## the new value is identical to previous one and in this case we could keep the cache
    cacheinv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) cacheinv <<- inv
  getinv <- function() cacheinv
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function returns the inverse of a matrix x (there is no check about whether x can inversed)
## It looks first whether the inverse was already calculated and return the cache value if it's the case
## If not, it will calculate the inverse, cache it and return the result.

cacheSolve <- function(x, ...)
{
## Get the cached value for the inverse of x, it makes the assumption that x
## has been made a special matrix by calling makeCacheMatrix (there is no verification)
  inv <- x$getinv()
## If there is a cached value return it
  if(!is.null(inv)) return(inv)
## Function will continute here if there is no cached value
## Get the matrix to inverse
  matrix <- x$get()
## Calculate the inverse using solve
  inv <- solve(matrix)
## Put inverse into the cache
  x$setinv(inv)
## Return the newly calculated inverse
  return(inv)
}
