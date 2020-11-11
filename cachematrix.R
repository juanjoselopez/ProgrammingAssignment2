## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#La siguiente función almacenará la inversa de una matriz
makeCacheMatrix <- function(x = matrix()) {
    
    # Primero se crea la variable caché vacía
    inversa <- NULL
    
    #En el siguiente código se establece el valor de la matriz
    set <- function(y){
        #El operador <<- permite a "set" instanciar x fuera del entorno de la función
        x <<- y
        inversa <<- NULL
    }
    
    #Obtener el valor de la matriz
    get <- function() x #retorna x
    
    #Obtener el valor de la inversa y almacenarla en "inversa"
    setinversa <- function(inv) inversa <<- inv
    
    #Recuperar el valor de la inversa
    getinversa <- function() inversa 
    
    #Enlistar las funciones para poder llamarlas luego
    list(set = set, get = get,
         setinversa = setinversa,
         getinversa = getinversa)
}

#La siguiente función obtendrá la inversa en caché
cacheInversa <- function(x, ...){
    
    #Obtener el valor retornado de la inversa
    inversa <- x$getinversa()
    
    #Condición de error
    #Si el valor del caché NO es nulo, obtener caché...
    if(!is.null(inversa)){
        message("Obteniendo Data en Cache")
        return(inversa)
    }
    #...Si ES nulo, entonces obtener la inversa (con solve()) y almacenarla en caché
    valor <- x$get()
    inversa <- solve(valor,...)
    x$setinversa(inversa)
    inversa #retornar la inversa
}
