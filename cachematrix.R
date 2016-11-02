# This programme aims to catche  the inverse of a Matrix and can be used when needed.
##----------------------------------------------------------------------------------##
#(Instruction for the assignment Matrix inversion is usually a costly computation and there may be some
#benefit to caching the inverse of a matrix rather than computing it
#repeatedly (there are also alternatives to matrix inversion that we will
#not discuss here). Your assignment is to write a pair of functions that
#cache the inverse of a matrix.)

## In this section, we have two functions
## 1. makeCacheMatrix
## 2.cacheSolve


#The first function, `makeCacheMatrix` creates a matrix, which is
#really a list containing a function to

#1.  set the value of the Matrix
#2.  get the value of the Matrix
#3.  set the value of inverse of the Matrix
#4.  get the value of inverse of the Matrix

   makeCacheMatrix <- function(x = matrix()) {
            InverVar <- NULL
            set <- function(y) {
                    x <<- y
                    InverVar <<- NULL
            }
            get <- function() x
            setInverVar <- function(inverse) InverVar<<- inverse
            getInverVar <- function() InverVar
            list(set = set, get = get, setInverVar = setInverVar, getInverVar = getInverVar)
    }
  

### Second Function - cacheSolve

cacheSolve <- function(x, ...) {
        InverVar <- x$getInverVar()
        if(!is.null(InverVar)) {
                message("getting cached data")
                return(InverVar)
        }
        data <- x$get()
        InverVar <- solve(data, ...)
        x$setInverVar(InverVar)
        InverVar
}


#Testing:
#t= matrix( c(2, 4, 1, 5),nrow=2, ncol=2)
#q<-makeCacheMatrix (t)
#q$get()
#      [,1] [,2]
#[1,]    2    1
#[2,]    4    5

#w<-cacheSolve(q)
#        [,1]       [,2]
#[1,]  0.8333333 -0.1666667
#[2,] -0.6666667  0.3333333
 
#solve(w)
 #     [,1] [,2]
#[1,]    2    1
#[2,]    4    5

