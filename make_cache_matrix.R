# I use `_` in my functions for readability and to catch typos

make_cache_matrix <- function(x = matrix()) 
{
    inv <- NULL
    set <- function(y)               #sets matrix
    {
        x <<- y
        inv <<- NULL
    }
    get <- function()               #gets matrix
        {x}                         
    set_inv <- function(inverse)    #sets inverse
        {inv <<- inverse}               
    get_inv <- function()           #gets inverse 
        {inv}
    list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
}



cache_solve <- function(x, ...)
{
    inv <- x$get_inv()  
    if(!is.null(inv))  # this skips calculation of `inv` has already been calculated
    {
        message("getting cached data")
        return(inv)
    }
    mtx <- x$get()
    inv <- solve(mtx, ...)          #calc inverse using `solve()`
    x$set_inv
    inv
}
