## R script made as part of a coursera course
## week 3, january 2017
## avanbeuz
## 
## Overal objective
## this script defines two functions as part of an excercise in order to assist in 
## calcualting the invers of a Matrix
## Similar to the vector example in the course excercise, this could be useful
## for very large matrices only.
## according to excercise we may assume that the matrix supplied is always invertible
##
## remark: I am notquite sure I understand this excercis in the scope of the 'apply' family

## makeCacheMatrix intended to define a data frame and set and get it and also it's inverse
## similar to the makeVector example in the excercise.

makeCacheMatrix <- function(x = matrix()) {
  #very similar to the MakeVector example in the excercise, but for a matrix
    inv_mat <- NULL
    set <- function(y){
      x <<- y
      inv_mat <<- NULL
    }
    get <- function() x
    
    set_cch_mat <- function(sol_mat) inv_mat <<- sol_mat
    get_cch_mat <- function() inv_mat
    list(set = set, get = get, set_cch_mat = set_cch_mat, get_cch_mat = get_cch_mat)
}


## cacheSolve calculates an inverse matrix useing the Sovle function. All very similar to
## the mean of vector example given in the lecture

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv_mat <- x$get_cch_mat()
    if(!is.null(inv_mat)){
      message("getting cached data")
      return(inv_mat)
    }
    data <- x$get()
    inv_mat <- solve(data)
    x$set_cch_mat(inv_mat)
    inv_mat      
}
