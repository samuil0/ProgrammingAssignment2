makeCacheMatrix <- function(mat=matrix()) {
        
        # Creating a null variable
        mat.inverse <- NULL
        
        # Creating a function to define the matrix
        set <- function(y) {
                
                mat <<- y
                mat.inverse <<- NULL
        }
        
        # This function returns the value that was defined for matrix
        get <- function() {
                mat
        }
        
        # Creating a function to define the inverse matrix
        set.inverse <- function(m=matrix()) {
                mat.inverse <<- m
        }
        
        # This function returns the value that was defined for inverse matrix
        get.inverse <- function() {
                mat.inverse
        }
        
        # Creating list to store functions
        list(set=set, get=get, set.inverse=set.inverse,
             get.inverse=get.inverse)
}

a <- makeCacheMatrix()

# Creating the matrix
z <- matrix(c(1,2,3,0,1,4,0,0,1),nrow = 3)

# Defining the matrix
a$set(z)

# Returning the matrix
a$get()

# Defining the inverse matrix
a$set.inverse(solve(z))

# Returning the inverse matrix
a$get.inverse()

cacheSolve <- function(x, ...) {
        
        # This code returns the value stored in the inverse matrix 
        mat.inverse <- x$get.inverse()
        
        # This condicional function identifies whether the matrix is NULL or if there is a value
        if(!is.null(mat.inverse)) {
                message('getting cached data')
                return(mat.inverse)
        }
        # This code returns the value stored in the inverse matrix  
        data <- x$get()
        
        # This code calculates the inverse function 
        mat.inverse <- solve(data, ...)
        
        # Defines the inverse matrix that was calculated as the 'mat.inverse' variable
        x$set.inverse(mat.inverse)
        mat.inverse
}


cacheSolve(a)


