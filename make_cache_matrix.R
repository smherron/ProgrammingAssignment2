make_cache_matrix <- function(x = matrix())
{
  inv <- NULL
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  set_inv <- function(inverse) 
  {inv <<- inverse}
  get_inv <- function() 
  {inv}
  list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
}

cache_solve <- function(x, ...)
{
  inv <- x$get_inv()
  if(!is.null(inv))
  {
    message("getting cached data")
    return(inv)
  }
  mtx <- x$get()
  inv <- solve(mtx, ...)
  x$set_inv
  inv
}
