# Function name: makeCacheMatrix.
# Description  : Creates a special-matrix object which can cache the inverse data.
# Inputs       : Takes a normal matrix object created from matrix() function. 
# Assumptions  : The input matrix provided is an invertible matrix.
#                No validation/checks are performed on the same.
makeCacheMatrix <- function(X=matrix()) {

    # Initialize inverse to NULL object upon new object creation.
    inverse <- NULL
    
    # Set function sets the given matrix.
    # Initialize inverse to NULL because original matrix changed.
    set_matrix <- function(Y) {
        X <<- Y
        inverse <<- NULL
    }
    
    # Get function to return the original matrix.
    get_matrix <- function()  X
    
    # Set inverse of the matrix.
    set_inverse <- function(I)  inverse <<- I
    
    # Get invesrse of the matrix.
    get_inverse <- function() inverse
    
    list(set_matrix = set_matrix, get_matrix = get_matrix, 
         set_inverse = set_inverse, get_inverse = get_inverse)
}


# Function name: cacheSolve.
# Description  : Performs the inverse operation on special-matrix object 
#                created by makeCacheMatrix().If the special-matrix inverse
#                is not cached it would compute the inverse of the matrix 
#                and caches the inverse for future fast retrieval.
#                Prints a message if inverse was retrieved from cache or computed.
#                Returns the inverse matrix of the original matrix.
# Inputs       : Takes a special-matrix object created from makeCacheMatrix() function. 
cacheSolve <- function(X=makeCacheMatrix()) {
    # try retrieving cached data.
    I <- X$get_inverse()
    
    if( ! is.null(I)) {
        message("Info: retrieved inverse from cache.")
    } else {
        message("Info: computing inverse now.")
        
        # get the original matrix.
        M <- X$get_matrix()
        
        # compute inverse of the matrix.
        I <- solve(M)
        
        # now cache the inverse.
        X$set_inverse(I)
        
    }
    # return the inverse which was either retrieved or computed.
    return(I)
}

# ---------------------
# Usage example:
# ---------------------
# > m  <- matrix(c(4,1,2,1)), 2, 2)
# > m
#       [,1] [,2]
# [1,]    4    2
# [2,]    1    1
# > ms <- makeCacheMatrix(m)
# > cinv <- cacheSolve(ms)
# Info: computing inverse now.
# > cinv <- cacheSolve(ms)
# Info: retrieved inverse from cache.
# cinv
#       [,1] [,2]
# [1,]  0.5   -1
# [2,] -0.5    2
#
# > m1 <- matrix(c(4,2,2,3), 2, 2) 
# > m1
#       [,1] [,2]
# [1,]    4    2
# [2,]    2    3
# > ms$set_matrix(m1)
# > cinv <- cacheSolve(ms)
# Info: computing inverse now.
# > cinv
#        [,1]  [,2]
# [1,]  0.375 -0.25
# [2,] -0.250  0.50

