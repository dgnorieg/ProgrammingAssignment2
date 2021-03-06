
## Crea un objeto matriz especial que puede almacenar en cach� su inverso
makeCacheMatriz <- function(x = matriz()) {
# Define la funci�n para establecer el valor de la matriz
# inverso del cach�
	inv <- NULL
	set <- function(y) {
	x <<- y
	inv <<- NULL
}
# Define la funci�n para obtener el valor de la matriz
get <- function() x
# Define la funci�n para establecer el inverso.
# no hay inverso en cach�
	setinversa <- function(inverse) inv <<- inversa
# Define la funci�n para obtener el inverso
	getinversa <- function() inv
# Devuelve una lista con las cuatro funciones anteriores
	list(set = set, get = get,
	setinversa = setinversa,
	getinversa = getinversa)
}


cacheSolve <- function(x, ...) {
	inv <- x$getinversa() # Esto recupera el valor almacenado en cach� para el inverso
	if (!is.null(inv)) { # Si el cach� no estaba vac�o, podemos devolverlo
	message("datos en cache")
	return(inv)
}

# El cach� estaba vac�o. Necesitamos calcularlo, almacenarlo en cach� y luego devolverlo.
mat <- x$get() # Obtener valor de matriz
	inv <- solve(mat, ...) # se calcula la inversa
	x$setinversa(inv) # resultado en cache
	inv # Regresa el inverso
}