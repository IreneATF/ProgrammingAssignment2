## PROGRAMMING ASSIGNMENT 2 - R PROGRAMMING
## Irene T - June 24th 2023
## This pair of functions can cache the inverse of a matrix

## This first function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      if(ncol(x)==nrow(x) && det(x)!=0){
            inve <- NULL
            set <- function(y) {
                  x <<- y
                  inve <<- NULL
            }
            get <- function() x
            setinverse <- function(inverse) inve <<- inverse
            getinverse <- function() inve
            list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
      
      }else{return(message("The matrix is not invertible."))}
}

## This second function computes the inverse of the previous matrix, 
## or retrieves the inverse from the cache if it has been previously calculated. 

cacheSolve <- function(x, ...) {
       ## Return a matrix that is the inverse of 'x'
      inve <- x$getinverse()
      if(!is.null(inve)) {
            message("getting cached data")
            return(inve)
      }
      data <- x$get()
      inve <- solve(data, ...)  ## calculating inverse
      x$setinverse(inve)
      inve
       
}

# setwd("C:/Users/irene/Documents/Lifelong learning/Learning R/R Learning Coursera/Coursera_Rprog_ass2/ProgrammingAssignment2")
# source("cachematrix.R")

## Testing the function with StackOverflow example
## x <-makeCacheMatrix(matrix(c(1,0,0,0,1,0,0,0,2),ncol=3,nrow=3))
## cacheSolve(x)
      #[,1] [,2] [,3]
#[1,]    1    0  0.0
#[2,]    0    1  0.0
#[3,]    0    0  0.5
## x$get()
      #[,1] [,2] [,3]
#[1,]    1    0    0
#[2,]    0    1    0
#[3,]    0    0    2
## x$getinverse()
      #[,1] [,2] [,3]
#[1,]    1    0  0.0
#[2,]    0    1  0.0
#[3,]    0    0  0.5

