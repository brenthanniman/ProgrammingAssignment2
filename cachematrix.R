## The following functions provide a new CacheMatrix class, which allows you
## to store the value of a matrix as well as it's inverse.  Provided is a make
## function to generate the CacheMatrix objects themselves, as well as a special
## cacheSolve function, extending the solve() function for matrix inversion of
## square, invertible matrices to use the CacheMatrix class. 

## makeCacheMatrix: Generates a special matrix class vector, allowing you
## to store and retrieve a matrix and its inverse through supplied methods.
## Allows for storage of both square and non-square matrices.
makeCacheMatrix <- function(x = matrix()) {
     ## x parameter represents the stored matrix 
     
     ## Inialize all required internal variables
     ## Inverse holds the matrix inverse
     Inverse <- NULL
     
     ## set: Method to assign a new value to the internal matrix
     set <- function(new_matrix = matrix()) {
          ## new_matrix parameter represents the new matrix to store
          
          ## Check if the new value is different from the old. If it is then
          ## store it. Otherwise, do nothing to maintain the cached data.
          if (!isTRUE(all.equal(new_matrix, x))) {
            ## Value is new: Store the new matrix in x and set the matrix
            x <<- new_matrix       
            ## Set the inverse to NULL, as the stored value no longer represents 
            ## the correct inverse
            Inverse <<- NULL
          }
          
          ## Return the value of the matrix
          invisible(x)
     }
     
     ## get: Method that returns the stored value of the matrix
     get <- function() x
     
     ## setInverse: Method to store the matrix inverse of the stored matrix x.
     ## This method also checks that the input parameter is truly the inverse of
     ## the stored matrix to maintain data consistency.
     setInverse <- function(new_Inverse) {
          ## new_Inverse parameter represents the new value of the matrix inverse
          
          ## Test that 'new_Inverse' truly represents the inverse.  If it does
          ## either left or right matrix multiplication of both matrices will 
          ## produce the identity matrix.  
          
          ## Test for the right inverse 
          Identity <- diag(nrow(x))     ## Generate the identity
          Product <- x %*% new_Inverse  ## Right-multiply the two matrices
          
          ## Test for equality of the product to the identity.
          rightInverse <- isTRUE(all.equal(Product, Identity))
          
          if (!rightInverse) { 
               ## If 'new_Inverse' is not the right inverse, check for a left inverse
               Identity <- diag(nrow(new_Inverse)) ## Generate the identity
               Product <- new_Inverse %*% x        ## Left Multiply
               
               ## Test for equality of the product to the identity.  
               leftInverse <- isTRUE(all.equal(Product, Identity))
               
               ## If it is also not a left inverse, then it doesn't represent
               ## the inverse of the stored matrix, so don't store it, let the
               ## user know and return a NULL.
               if(!leftInverse) {
                    message("Inverse not cached: The input matrix does not 
                         represent the inverse of the stored matrix.")
                    return(invisible(NULL))
               }
          } 
          
          ## The matrix is either a right or left matrix, so store it.
          Inverse <<- new_Inverse
          return(invisible(Inverse))
     }
     
     ## getInverse: Method to return the stored matrix inverse
     getInverse <- function() {
          ## Verify that the inverse has already been set. 
          if (is.null(Inverse)) {
               ## The inverse has not been cached yet, so let the user know.
               message("Inverse not yet cached.")
               return(NULL)
          } else {
               ##Return the already stored inverse.
               Inverse     
          }
     } 
     
     ## Returns the specialized matrix Vector object, exposing the methods for
     ## modifying the data
     list (set = set, 
           get = get, 
           setInverse = setInverse, 
           getInverse = getInverse)
}


##cacheSolve: This function provides the inverse of a square, invertible matrix
##stored in a special matrix class CacheMatrix, which allows for storage of the 
##matrix inverse.  If the result has already been cached, it returns the cached 
##result, otherwise it recalculates it with the solve() function.  
cacheSolve <- function(x, ...) {
     ## parameter x is a CacheMatrix matrix holding the matrix to be inverted
     
     ## Test to see if the inverse has already been cashed.
     if(!is.null(x$getInverse())){
          ##Inverse already cached, let the user know that the cached data is used
          message("Getting cached data.")
     } else {
          ##Inverse has not been cashed, recalculate and store the inverse
          message("Calculating new inverse.")
          Inverse <- solve(x$get(),...)
          x$setInverse(Inverse)
     }
     
     ## Return the inverse
     return(x$getInverse())
}
