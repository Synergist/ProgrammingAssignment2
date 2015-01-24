## R Programming - 010 | Coursera
## Data Science Specialization - Johnss Hopkins University
## Programming Assignment 2
## Author: Pranav Singh
## Date: 01/21/2015
## Description: 
##  Matrix inversion is typically a costly computatiion. If we expect to need a
##  matrix's inverse multiple times, rather than computing it repeatedly, we can
##  encompass a matrix in an object that also caches its inverse. The functions
##  makeCacheMatrix and cacheSolve implement this feature.

## Creates a special "matrix" object (lets call it a CacheMatrix) that caches its inverse.
## It returns a list with the following 4 functions as its elements:
##      "set" : initializes a CacheMatrix object
##      "get" : returns the regular matrix object stored in the CacheMatrix
##      "setInverse" : caches the inverse of the matrix that the CacheMatrix object encompasses
##      "getInverse" : returns the cached inverse 
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) inverse <<- inv
    getInverse <- function() inverse
    list(set = set, get = get, getInverse = getInverse, setInverse = setInverse)
}


## Finds the inverse of x, which is assumed to be a CacheMatrix object that
## encompasses an invertible matrix. If the inverse is already cached within the
## CacheMatrix obbject, then it is simply returned without having to recompute it.
## Otherwise, the inverse is computed with the standard solve() function, and is
## then cached within the CacheMatrix x object for potential subsequent calls.
## Note: it returns the inverse as a regular matrix object. 
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if (!is.null(inv)) return(inv)
    else {
        inv <- solve(x$get(), ...)
        x$setInverse(inv)
        return(inv)
    }
}
