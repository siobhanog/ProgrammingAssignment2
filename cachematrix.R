## Put comments here that give an overall description of what your
## functions do

## cachematrix comprises two functions makeMatrix and cacheInverse.
## code creates a matrix based on data provided to makeMatrix and then creates argument list that will
## be passed to cacheMatrix to determine the inverse using inbuilt solve function
## cacheMatrix checks to see if inverse has already been calculated. If inverse exists it is retrieved from cache and returned as result
## otherwise inverse is calculated and then returned as result 

## Write a short comment describing this function

## instructions - Write funtcion that creates a special "matrix" object that can cache its inverse 

## Creates a set of functions and then returns them as an argument list to the parent environment
## Initalizes the inverse matrix and defines the set() function 
## Defines the behaviors for makeMatrix objects and returns them in arg list


makeCacheMatrix <- function(x = matrix()) {

  matrix_inverse <- NULL
  
## assign the set function's argument y to the parent environment object x   
  
  set <- function(y) matrix_inverse <-  {
    
    x <<- y
    
## initalize the matrix-inverse to an empty matrix
## this  clears any prior values of matrix_inverse from cache 
    
    matrix_inverse <<- NULL
    
  }
  
  
## getter function tp retrieve value of x from the parent environment  
  
  get <- function() x 
  
## setter function to retrieve value of "inverse"(function name) from the parent environment  
  
  setinverse <- function(inverse) matrix_inverse <<- inverse
  
## getter function to retireve correct value of the inverse matrix 
  
  getinverse <- function() matrix_inverse
  
  
## create and return the list of function arguments to be used by cacheInverse function below  
  
  list(set = set, get = get, setinverse=setinverse, getinverse = getinverse)
  
  
  
  
  
}


## Write a short comment describing this function

## Instructions : Write a function to compute the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

## cacheSolve takes as its argument the output of function makeMatrix


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

##  pull in function arg that stipulates what function we are calculting in this case the matrix_inverse  
  
    matrix_inverse <- x$getinverse()
    
## check if the matrix_inverse has already been calculated and if it exists in cache return its value
    
    
    if(!is.null(matrix_inverse)) {
      
      message("getting cached inverse data")
      return(matrix_inverse)
    }

## otherwise compute the inverse for the matrix and return that value
    
    data <- x$get()
    
    matrix_inverse <- solve(data,...)
    x$setinverse(matrix_inverse) 
    matrix_inverse
    
    
  }
  
  

