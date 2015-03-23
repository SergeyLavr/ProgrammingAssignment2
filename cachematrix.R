makeCacheMatrix <- function(matrix = matrix()) {
    # Creates a type of matrix which may cache its inversed matrix
    # @param matrix - square matrix used to initialize content
    # @return - a type with defined operations:
    #	 setMatrix(newMatrix) - sets the new matrix value
    #	 getMatrix() - gets the matrix value
    #	 setInversedMatrix(newInversedMatrix) - sets the new inversed matrix value
    #	 getInversedMatrix() - gets the inversed matrix value

    if (is.null(matrix))
        stop("NULL argument provided")

    # Check if provided matrix is square matrix
    matrixDimensions<-dim(matrix)
    if (matrixDimensions[1] != matrixDimensions[2])
        stop("Cannot operate with non-square matrixes.")
    
    if (matrixDimensions[1] == 0)
        stop("Cannot operate with 0x0 matrixes.")
    
    # This variable will be used as a cache
    inversedMatrix <- NULL

    # This function will be used for setting new matrix value
    setMatrix <- function(newMatrix) {
        matrix <<- newMatrix
        inversedMatrix <<- NULL
    }

    # This function will be used for getting matrix value
    getMatrix <- function() matrix 

    # This function will be used for setting inversed matrix value to be cached
    setInversedMatrix <- function(newInversedMatrix) inversedMatrix <<- newInversedMatrix

    # This function will be used for getting cached inversed matrix value
    getInversedMatrix <- function() inversedMatrix

    list(setMatrix = setMatrix, 
         getMatrix = getMatrix,
         setInversedMatrix = setInversedMatrix,
         getInversedMatrix = getInversedMatrix)
}

cacheSolve <- function(matrix, tol = .Machine$double.eps) {
    # Finds the inversed matrix for provided matrix
    # @param matrix - a matrix type created and initialized using makeCacheMatrix function
    # tol - the tolerance for detecting linear dependencies in the columns of matrix. The default is .Machine$double.eps. Not currently used with complex matrices.
    
    if (is.null(matrix))
        stop("NULL argument provided")

    inversedMatrix<-matrix$getInversedMatrix()
    if (!is.null(inversedMatrix))
        return(inversedMatrix)
    
    matrixDimensions<-dim(matrix$getMatrix())
    if (matrixDimensions[1] != matrixDimensions[2])
        error("Cannot compute inverse for non-square matrix.")

    identityMatrix<-diag(matrixDimensions[1])
    inversedMatrix<-solve(matrix$getMatrix(), identityMatrix, tol=tol)
    matrix$setInversedMatrix(inversedMatrix)
    inversedMatrix
}
