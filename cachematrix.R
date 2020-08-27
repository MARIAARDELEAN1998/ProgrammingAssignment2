## R programming - Assignment 2 - Lexical scoping


## The makeCacheMatrix() function contains a series of 'setter' and 'getter'
## nested functions that are used to set the values of the input matrix and inverse matrix.
## An object of type makeCacheMatrix() is created and each function is assigned
## an element of a list that will be returned to the parent environment.
## An object of type makeCacheMatrix()is used as an input for the CacheSolve() function. 

makeCacheMatrix <- function(x = matrix()) {
        #The inverse matrix initialized in the makeCacheMatrix() environment        
        inverse.matrix <- NULL 
        
        #Setter function: 
        #set.input.matrix() is used to control the changes and change the values 
        #of the input matrix within the parent environment of makeCacheMatrix()
        #and to delete any value of the inverse matrix that could have been cached previously
        set.input.matrix <- function(input.matrix) 
        {
                x <<- input.matrix
                inverse.matrix <<- NULL
        }
        
        #Getter function:
        #get.input.matrix() retrieves the value of the input matrix from within 
        #the parent environment of makeCacheMatrix()
        get.input.matrix <-function() x
        
        #Setter function:
        #set.inverse.matrix() is used to define the inverse matrix in the parent
        #environment of makeCacheMatrix()
        set.inverse.matrix <- function(output.matrix) inverse.matrix <<- output.matrix
        
        #Getter function:
        #get.inverse.matrix() retrieves the value of the input matrix from the parent
        #environment of makeCacheMatrix()
        get.inverse.matrix <- function() inverse.matrix
        
        #List definition:
        #Returns a named list that describes the object of type makeCacheMatrix() 
        list(set.input.matrix = set.input.matrix, 
             get.input.matrix = get.input.matrix,
             set.inverse.matrix = set.inverse.matrix,
             get.inverse.matrix = get.inverse.matrix)
}


## The cacheSolve() function gets as an input an object of type makeCacheMatrix()
## First, it checks the existence of a cached inverse matrix in the makeCacheMatrix()object
## If the cached matrix is not found, then it calculates the inverse matrix and returns it to 
## the parent environment.

cacheSolve <- function(x, ...) {
        # inverse.matrix is assigned the value of the inverse matrix from the makeCacheMatrix() object
        # which is passed as an argument for the cacheSolve() function
        inverse.matrix <- x$get.inverse.matrix()
        
        #the if statement checks if there is a cached inverse matrix that could be 
        #returned to the parent environment of cacheSolve()
        if(!is.null(inverse.matrix))
        {
                print("Retrieving cached inverse matrix")
                return(inverse.matrix)
        }
        
        #input.matrix1 is assigned the value of the input matrix from the makeCacheMatrix() object
        input.matrix1 <- x$get.input.matrix()
        
        #inverse.matrix is assigned the value of the inverse marix of the input matrix.
        # the function solve() is used to calculate the inverse of the input matrix
        inverse.matrix <- solve(input.matrix1)
        
        #the set.inverse.matrix() function is used to set the inverse matrix in the input
        #makeCacheMatrix() object
        x$set.inverse.matrix(inverse.matrix)
        
        #the value of the inverse matrix is returned to the parent environment and printed
        inverse.matrix
        
}
