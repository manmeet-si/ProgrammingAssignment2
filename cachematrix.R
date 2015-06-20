## makeCacheMatrix function takes a single numeric data type argument 
## which is a matrix. It has a getter and setter namely get and set
## There are getter and setter methods for the inverseComputed for 
## these matrices.
## Due to the constraint of inverse the input matrix has to be a square matrix

##cacheSolve function will be the entry point for input where it would first see if the 
## result for this is already stored in makeCacheMatrix, if not it would use solve function
## to computer the inverse and set it and finally return the inverse.


## Write a short comment describing this function

makeCacheMatrix <- function(mat = numeric()) {
    matInv <- NULL
    set <- function(matt) {
        mat <<- matt
        matInv <<- NULL
    }
    get <- function()
        mat
    setMatrixInverse <- function(matInvv)
        matInv <<- matInvv
    getMatrixInverse <- function()
        matInv
    list(set = set, get = get,
         setMatrixInverse = setMatrixInverse,
         getMatrixInverse = getMatrixInverse)
}

cacheSolve <- function(func, ...) {
    matInv <- func$getMatrixInverse()
    if(!is.null(matInv)) {
        message("getting cached data")
        return(matInv)
    }
    data <- func$get()
    matInv <- solve(data)
    func$setMatrixInverse(matInv)
    matInv
}
