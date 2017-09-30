## Developer: J. Pi
## Date: 9/30/2017
## 
## Instructions
##
## 1. Create a simple square matrix (must be square matrix for the program to work)
##    ex. testmatrix <- matrix(rnorm(4), 2)   ## creates a 4x4 matrix
## 2. Create a cached list of functions and pass the matrix using the makeCacheMatrix function
##    ex. testlist <- makeCacheMatrix(testmatrix)
## 3. Confirm that you have values stored on testlist
##    testlist$get()   ## will print to screen the original matrix
##    testlist         ## will print to screen the set, get, setinverse and getinverse functions
## 4. Compute the matrix inverse
##    cacheSolve(testlist)   ## will print to screen the inverted matrix
## 5. Run the cacheSolve(testlist) a few more times - if you like
##    cacheSolve(testlist)   ## will print to screen "getting cached data" as m is no longer NULL
##


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL             ## sets m to NULL
    set <- function(y){   ## sets set function
        x <<- y           ## sets x to equal y in the parent environment 
        m <<- NULL        ## sets m to equal NULL in the parent environment 
    }
    get <- function() x    ## sets the get function  
    setinverse <- function(inverse) m<<- inverse  ## sets setinverse function
    getinverse <- function() m   ## sets getmatrix function 
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) ## setsup list of functions to be used
}

cacheSolve <- function(x, ...) {  ## takes in the values of testlist (functions created in makeCacheMatrix)
    m <- x$getinverse()           ## passes the value of the original matrix to m
    if(!is.null(m)){              ## **runs the second time as value of m is not NULL**
        message("getting cached data")  ## displays on the second run while using cached data
        return(m)                       ## returns the value of m if data is cached and stops execution
    }
    matrix <- x$get()             ## pulls the values of x$get into matrix - **runs the first time as value of m is NULL**
    m <- solve(matrix, ...)       ## puts the values of the inversion from matrix into m
    x$setinverse(m)               ## move the value of x$inverse to m
    m                             ## displays m
}