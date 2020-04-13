makeCacheMatriz <- function(x = matriz()) {
inv <- NULL
set <- function(y) {
x <<- y
}
get <- function() x
setinversa <- function(inverse) inv <<- inversa
getinversa <- function() inv
list(set = set,
get = get,
setinversa = setinversa,
getinversa = getinversa)
}

cachesolucion <- function(x, ...) {
inv <- x$getinversa()
if (!is.null(inv)) {
message("datos en cache")
return(inv)
}
mat <- x$get()
inv <- solve(mat, ...)
x$setinversa(inv)
inv
}