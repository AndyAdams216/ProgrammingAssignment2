# Creates a matrix and its inverse
makeCacheMatrix <- function(m_matrix=matrix()){
    
    m_inverse <- NULL

    # store the matrix and reset its inverse
    set <- function(matrix) {
        m_matrix <<- matrix
        m_inverse <<- NULL
    }

    # access the matrix
    get <- function() m_matrix

    # sets the inverse of the matrix to 'inverse' if variable unbound 
    # and available in environment
    setInverse <- function(inverse) m_inverse <<- inverse

    # access the inverse of the matrix
    getInverse <- function() m_inverse

    # return the cached matrix as a list
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


# Calcualtes the inverse of a given matrix and caches the result for future use
cacheSolve <- function(cachedMatrix) {
    
    # attempt to get cached inverse
    inverse <- cachedMatrix$getInverse()   
    if(!is.null(inverse)) {
        message("Found cached matrix inverse.")
        return(inverse)
    }
    
    # if no cached version calculate one
    matrix <- cachedMatrix$get() 
    # matrix must be square
    if( ncol(matrix) != nrow(matrix) ){
        message("Matrix is not square.")
    }   
    else if( det(matrix) == 0){
        message("Cannot invert matrix with a determinant of zero.")
    }
    else{
        # calculate inverse of matrix
        inverse <- solve(matrix)
        # cache matrix inversion
        cachedMatrix$setInverse(inverse)
        return(inverse)
    }
}   