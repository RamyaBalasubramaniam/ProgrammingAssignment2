## These functions use the lexical or static scoping rules of R programming
## to optimize the time required for a tedious computation if it has been 
## already been done and saved in the cache memory


## This function first initiallises the value of variables storing the matrix 
## and its Inverse to NULL. Then it describes the functions set(y), get(), 
## setSolve(solve) and getSolve(). Set(y) function can assign a new Square 
## matrix and reset the value of Inverse to NULL. The function get() returns 
## the value of matrix as stored in the cache memory. setSolve(solve) function 
## essentially returns its argument itself and getSolve() function returns 
## the value of variable storing the Inverse of the matrix as stored in the memory.
## this function makeCacheMatrix returns a list of the four functions described
## in this function.

makeCacheMatrix <- function(x = matrix()) 
{
    I <- NULL             
    set <- function(y)                                          
    {
        x <<- y
        I <<- NULL
    }
    get <- function() x                                         
    setSolve <- function(solve) I<<- solve                      
    getSolve <- function()  I                                   
    list(set=set, get=get, setSolve=setSolve, getSolve=getSolve)
}


## This function takes the list x returned by the makeCacheMatrix function as one of it's 
## argument. "..." represents the arguments which could be added later to this function.
## It accesses the member function of the list x, getSolve() to retrive value of variable 
## used for Inverse as stored in memory. If the value of this variable is not NULL then
## Inverse has already been calculated and this value is returned. Otherwise the function 
## accesses the member function of the list x get() to retrive value of matrix and then 
## finds its inverse and stores it in cache memory so that it can be accessed in future.
## This function cacheSolve returns the value of the Inverse of the matrix.

cacheSolve <- function(x, ...) 
{
     I<- x$getSolve() 
     if(!is.null(I))
     {
         message("getting cached data")
         return(I)
      }
      data <- x$get()
      I<- solve(data)
      x$setSolve(I)
      I
}
