## The cachematrix.R file contains two functions, makeCacheMatrix() 
## and cacheSolve (). The first function in the file, makeCacheMatrix() 
## creates an R ## object that stores a matrix and its inverse. 
## The second function,  cacheSolve() requires an argument that is returned 
## by makeCacheMatrix() in order ##to retrieve the inverse from the cached 
## value that is stored in the  by makeCacheMatrix() object's environment.


## ## The first thing that occurs in the function is the initialization 
## of two objects, x and inv. x is initialized as a function argument, 
## so no further initialization is required within the function. m is set 
## to NULL, initializing it as an object within the  makeCacheMatrix() 
## environment to be used later.

makeCacheMatrix <- function(x = matrix()) {         
  inv <- NULL     #initializing inverse as NULL         
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## Within set() we use the <<- operator, which assigns the value on the 
  ## right side of the operator to an object in the parent environment 
  ## named by the object on the left side of the operator. When set() is 
  ## executed, it does two things:
  ## 1) Assign the input argument to the x object in the parent environment, and
  ## 2) Assign the value of NULL to the inv object in the parent environment. 
  ## This line of code clears any value of inv that had been cached by a prior  
  ## execution of cachesolve().
  
  get <- function() x   # function to get matrix x
  ##  This function takes advantage of the lexical scoping features in R. 
  ## Since the symbol x is not defined within get(), R retrieves it from the 
  ## parent environment of  makeCacheMatrix ().
  
  setinv <- function(inverse) inv <<- inverse
  ## Here,  makeCacheMatrix () defines the setter for the inverse inv. 
  ## Since inv is defined in the parent environment and we need to access it  
  ## after setinv() completes, the code uses the <<- form of the assignment operator to assign the input argument to the value of inv in the parent ## environment.
  
  
  getinv <- function() inv
  ## Finally,  makeCacheMatrix () defines the getter for the inverse inv.   
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}
##  The last section of code assigns each of these functions as an element 
## within a list(), and returns it to the parent environment.



##   cacheSolve () is required to populate and/or retrieve the inverse from 
## an object of type  makeCacheMatrix ().

cacheSolve <- function(x, ...) {   ## gets cache data
  inv <- x$getinv()
  ## the function attempts to retrieve an inverse from the object passed in as 
  ## the argument. First, it calls the getinv() function on the input object
  
  if(!is.null(inv)){      #checking whether inverse is NULL
    message("getting cached data!")
    return(inv)    #returns inverse value        
  }
}
  ##  Then it checks to see whether the result is NULL. Since  
  ## makeCacheMatrix () sets the cached inverse to NULL whenever a new vector 
  ## is set into the  ##object, if the value here is not equal to NULL, 
  ## we have a valid, cached inverse and can return it to the parent environment
  ## If the result of !is.null(m) is FALSE,  cacheSolve() gets the vector 
  ## from the input object, calculates an inverse , uses the setinverse() 
  ## function ##on the input object to set the inverse in the input object, 
  ## and then returns the value of the inverse to the parent environment by 
  ## printing the ##inverse object.
  
