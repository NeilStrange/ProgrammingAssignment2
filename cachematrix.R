#------------------------------------------------------------------------------
# Coursera: R Programming Assignment 2 
#
# Author: Neil Strange
# Date: 27.09.2015
#
# References: R Programming, Assignment 2 Instructions: Example Code (R Peng)
#
# Instructions for Use:
#   There are two functions: makeCacheMatrix() and cacheSolve().
#   Usage is as follows:
#      1. Create x to hold a cached matrix,  x <- makeCacheMatrix(m) Where 'm' 
#         is a matrix object passed to the function. 'm' MUST BE A SQUARE 
#         matrix (nrow = ncol) and not singular (i.e. not invertible, a solution 
#         can be found).
#      2. To solve the matrix m, producing an inverse and storing the result
#         in a cache use x_inv <- cacheSolve(x).
#
#   Example use:
#      m <- matrix(1:4, nrow = 2, ncol = 2)
#      m
#             [,1] [,2]
#        [1,]    1    3
#        [2,]    2    4
#      x <- makeCacheMatrix(m)
#      x_inv <- cacheSolve(x)
#      x_inv
#             [,1] [,2]
#        [1,]   -2  1.5
#        [2,]    1 -0.5
#
#  Note that the matrix must be square to be solved and must have a solution
#  (must be invertible) otherwise an error will be flagged.
#------------------------------------------------------------------------------


makeCacheMatrix <- function(x = matrix()) {
#------------------------------------------------------------------------------
# This function is a factory - it creates an object from the matrix passed via
# the function parameter x, which is a physical matrix. It creates a set of 
# functions exposed as a list of x (get, setinverse, getinverse). It holds the 
# solution (inverse) of x in the variable 'm' which is exposed through the 
# x$getinverse() function.
#
# When the functionality around x is called to solve a matrix the results are
# stored in a buffer 'm'. These results can be retrieved in place of a fresh
# recalculation of the solution, saving time in case of the need to refer to
# the inverse multiple times overa piece of code. This also has the added 
# advantage that the cache is held within x and not as a value in the
# environment, and so is less likely to be overwritten. The buffer is also 
# set to NULL when a new makeCacheMatrix is called making it less likely that
# results of a previous calculation will be used in error.
#
# Usage: 
#   x <- makeCacheMatrix(m), where m is a matrix, x will contain a list of
#   functions get, setinverse, getinverse. The values of the matrix are held
#   inside x but only visible to external code through x$get().
#------------------------------------------------------------------------------

# m contains the inverse of the matrix passed to x 
# as makeCacheMatrix is being called afresh, nullify m to overwrite any previous
# value held in the environment
  m <- NULL
  
# define functions that will be made available through x-----------------------

  # get retrieves the matrix originally passed to makeCacheMatrix
  get <- function() x
  
  # setinverse saves a passed solution of the matrix x to the cache m
  setinverse <- function(solve) m <<- solve

  # getinverse retrieves m from the cache, the solution to matrix x
  getinverse <- function() m

# define a list of function names which will be visible as x$get()...----------
  list(get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

cacheSolve <- function(x) {
#------------------------------------------------------------------------------
# Return a matrix that is the inverse of 'x'
# x must have been created using the makeCacheMatrix() function.
# 
# cacheSolve() returns an error if the matrix in x isn't square or is not 
# solvable (i.e. not invertible).
#
# cacheSolve() will retrieve a previously calculated solution if one exists
# in x, this is available through the x$getinverse() function which returns
# null if no previous calculation result is available.
#------------------------------------------------------------------------------

# get the inverse solution of x from the cache, if one exists then return 
# the cached solution m as the result of the cacheSolve() function
  m <- x$getinverse()
  if(!is.null(m)) {
    return(m)
  }
# if a solution was not in the cache then calculate one (retrieve the matrix, 
# solve it and save it to the cache. Return the solution m as the result of the 
# cacheSolve() function
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}