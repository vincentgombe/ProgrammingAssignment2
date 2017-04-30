## Below is a pair of functions , makeCacheMatrix and cacheSolve, which in combination
## compute the inverse matrix of an input invertible square matrix and store/cache the result 
## for faster access in subsequent calls without recalculatiion if the matrix remains unchanged.
## Should a different matrix be input,a new inverse matrix is computed,the old result 
## is cleared from "memory" and the new one stored.



## makeCacheMatrix creates special matrix object that caches the inverse of an input invertible square matrix.
## A set of functions are built and passed to the parent environment as a list object.
## The objects that store the information within makeCacheMatrix, x and inv are initialised in the parent environment of the makeCacheMatrix function
## The set() function uses the <<- assignment operator to assign the input argument to object x that was initialised as empty matrix in the parent environmrnt.
## Whenever x is reset, the value of the inv object is also reset to NULL,thereby clearing any value that may have been stored from earlier execution of the cacheSolve
## Getters and setters for the two data objects in makeCacheMatrix are then defined which are then as named elements of list()and passed to the parent environment

makeCacheMatrix <- function(x = matrix()) {
 	inv <- NULL
 	set <- function(y) {
 		x <<- y
 		inv <<- NULL
 		}
 			get <- function() x
 			setinv <- function(solve) inv <<- solve
 			getinv <- function() inv
 			list(set = set, get = get, setinv = setinv, getinv = getinv)
  }



## cacheSolve() populates or retrieves the inverse from an object of type makeCacheMatrix. 
## It requires an input arguement x of type makeCachemean
## When an input matrix is passed, and the inverse has previously been computed then this is returuned to the parent environment without recomputation.
## Otherwise, if it's a new matrix object whose inverse is not in the cache, a new inverse is computed and returned to the parent environment.

cacheSolve <- function(x,...) {
        ## Return a matrix that is the inverse of 'x'
	 inv <- x$getinv()
	if (!is.null(inv)){
		message("getting cached inverse")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data,...)
	x$setinv(inv)
	inv
}

## Quick check of the functions##

#m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
#m1
 #     [,1]  [,2]
#[1,]  0.50 -1.00
#[2,] -0.25  0.75
# TestMatrix_object <- makeCacheMatrix(m1)
# cacheSolve(TestMatrix_object)
 #    [,1] [,2]
#[1,]    6    8
#[2,]    2    4

#calling with the same input matrix retrieves, rather than computes the inverse
#cacheSolve(TestMatrix_object)
#getting cached inverse
  #   [,1] [,2]
#[1,]    6    8
#[2,]    2    4

#input a differnt matrix using the set()
#n2 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2)
#TestMatrix_object$set(n2)
#and obtain its matrix inverse by
#cacheSolve(TestMatrix_object)
#     [,1] [,2]
#[1,]    3    7
#[2,]    1    5

# Again, a subsequent call with second matrix retrieves, rather than computes the inverse
# cacheSolve(TestMatrix_object)
#getting cached inverse
 #    [,1] [,2]
#[1,]    3    7
#[2,]    1    5


                      #REFERENCES#

#1. Leonard Greski ,Demystifying MakeVector(), retrieved April 29, 2017.
#2. Alan E berger, Simple test matrices for the lexical scoping programming assignment, retrieved April 29, 2017
