
## Crea un objeto matriz especial que puede almacenar en caché su inverso
makeCacheMatriz <- function(x = matriz()) {
# Define la función para establecer el valor de la matriz
# inverso del caché
	inv <- NULL
	set <- function(y) {
	x <<- y
	inv <<- NULL
}
# Define la función para obtener el valor de la matriz
get <- function() x
# Define la función para establecer el inverso.
# no hay inverso en caché
	setinversa <- function(inverse) inv <<- inversa
# Define la función para obtener el inverso
	getinversa <- function() inv
# Devuelve una lista con las cuatro funciones anteriores
	list(set = set, get = get,
	setinversa = setinversa,
	getinversa = getinversa)
}


cacheSolve <- function(x, ...) {
	inv <- x$getinversa() # Esto recupera el valor almacenado en caché para el inverso
	if (!is.null(inv)) { # Si el caché no estaba vacío, podemos devolverlo
	message("datos en cache")
	return(inv)
}

# El caché estaba vacío. Necesitamos calcularlo, almacenarlo en caché y luego devolverlo.
mat <- x$get() # Obtener valor de matriz
	inv <- solve(mat, ...) # se calcula la inversa
	x$setinversa(inv) # resultado en cache
	inv # Regresa el inverso
}