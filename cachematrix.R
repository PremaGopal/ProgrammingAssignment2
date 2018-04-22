## The purpose of this function is to calculate the inverse of a matrix
## and return the inverse of the matrix from either the cache (done first)
## or through calculation. For this project a 2x2 matrix was used for testing
## this example matrix will be stored as "mtrix"
mtrix<-matrix(1:4,2,2)

## MakeCache matrix is a function which creates a special matrix containing
## the function to:
##1. set the value of the matrix
##2. get the value of the matrix
##3. Set the value of the inverse of the matrix
##4. Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()){
  ##set the intial value of matrix to NULL
  inv <- NULL 
  ##sets and gets the origin matrix
  setorigin <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getorigin <- function() x
  ##sets and gets the inverse matrix
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  ##Makes a list
  list(set = setorigin, get = getorigin,
       setinverse = setinverse,
       getinverse = getinverse)
  }
#Now let us insert "mtrix" into this function and store as "ctrix"
ctrix<-makeCacheMatrix(mtrix)

## This function checks to see if the inverse has been caculated. 
## If so the inverse is returned from the cache. 
## Otherwise the inverse is calculated via the "solve" function
cachesolve<- function(x,...){
  #gets inverse from cache and assign to variable "inv"
  inv<- x$getinverse()
  #If value is not NULL retrieves inverse from cache
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  #Otherwise get origin matrix and calculate inverse and set
  origin <- x$get()
  inv<- solve(origin,...)
  x$setinverse(inv)
  #Display the result! :)
  inv
}
#Now back to our example matrix. Insert "ctrix" into function
cachesolve(ctrix)

#You should get this as the result 
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#You can also test your own matrix






