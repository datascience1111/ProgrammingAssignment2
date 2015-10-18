##makeCacheMatrix                             ##function creates special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {   ##creates makeCacheMatrix special function 
    m<- NULL                                  ##assings NULL value to object m
    set<- function(y){                        ##creates funtion y and assigns it to set
        x <<- y                               ##assigns y value to x object for use in other environments
        m<<- NULL                             ##assigns NULL value to m for use in other environments
    }
    get <- function() x                       ##assings function() x to object get
    setmatrix<- function(matrix) m <<- matrix ##assigns matrix (from other environment) to used by funtion(matrix) and assigns value to setmatrix object
    getmatrix<- function() m                  ##assigns function() m to getmatrix object
    list(set = set, get = get, 
         setmatrix = setmatrix, 
         getmatrix = getmatrix)               ##creates a list containing objects are set to their respective values
}

#cacheSolve                                   ##computes the inverse of the special "matrix"

cacheSolve <- function(x, ...) {        
    m<- x$getmatrix()                         ##assigns x$getmatrix atomic value to object m
    if(!is.null(m)){                          ##checks to see if matrix has not already been established
        message("getting cached data")        ##prompts user function is getting cached matrix value
        return(m)                             ##returns cached matrix m to user
    }
    data<-x$get()                             ##if matrix is null, assigns function() x to data object, which was previously assigned to get 
    m<-matrix(data, ...)                      ##assigns matrix using data (and any other arguments passed) to object m 
    x$setmatrix(m)                            ##selects x$setmatrix (previous assigned the function called matrix)
    solve(m)                                  ##returns inverse of matrix m
    
    
}
