###
### ASIGNACION DE SIMULACIONES
###
### Integrantes:
### -Albert Acevedo CI: 26.710.983
### -Silvio Bigotto CI: 25.161.762
### -Gabriela Bracamonte CI: 24.618.141
### -Daniela Cadenas CI: 25.293.565
### -Gabriel Roa CI: 25.919.459
### 

### 3) Desarrollar el ejemplo del lanzamiento de monedas de la página 873 del liberman, use un software de su
### preferencia para programación del mismo (Debe adjuntar los programas con la entrega de proyecto).

### PARA ESTO, SE IMPLEMENTA LA FUNCIÓN simulacion_dados.
### Esta recibe como parámetro la cantidad de veces que va a jugar y un valor booleano para determinar si imprime todos los datos o sólo el resumen.

simulacion_dados <- function(niter,imprimir){ 
    i = 0 # Iteración nro. i
    victorias = 0 # Contador de veces que ha ganado
    saldo_global = 0 # Saldo global (acumula todas las veces que ha jugado)
    prom_lanzamientos = 0 # Cantidad promedio de lanzamientos antes de que cada juego termine
    prom_ganancias = 0 # Cantidad promedio que "gana" cada vez que juegue (puede ser negativo)
    while (i<niter){ # Ejecuta por la cantidad de iteraciones;
        saldo = 8 # Saldo inicial
        diferencia = 0 # La diferencia entre caras y sellos
        ncaras = 0 # Número de caras
        nsellos = 0 # Número de sellos
        if (imprimir) cat("\n")
        while (abs(diferencia)<3){ # Si caras - sellos está entre -3 y 3:
            n = floor(abs(runif(1, min=0, max=2))) # Genera un número entero aleatorio entre 0 y 1. 0 es cara, 1 es sello.
            saldo = saldo - 1 # Gastas $1 para jugar.
            if (n==0){ # Si sacas cara...
                ncaras = ncaras + 1 # Aumenta la cantidad de caras.
                if (imprimir) cat("C")
            } else { # De lo contrario, sacas sello...
                nsellos = nsellos + 1 # Ídem
                if (imprimir) cat("S")
            }
            diferencia = ncaras - nsellos # Se calcula la diferencia al final para determinar si sigue o no.
        }
        saldo_global = saldo_global + saldo # Si no sigue, suma el saldo actual (sea positivo o negativo) al saldo global
        prom_lanzamientos = (prom_lanzamientos + ncaras + nsellos) / 2 # Promedio de lanzamientos
        prom_ganancias = (prom_ganancias + saldo) / 2 # Promedio de ganancias
        if (saldo<0){ # Si perdiste dinero...
            if (imprimir) cat(sprintf("\nHAS PERDIDO. \nDespués de lanzar el dado %s veces, perdiste un total de $%s",ncaras+nsellos,abs(saldo)))
        } else { # Si ganaste dinero...
            if (imprimir) cat(sprintf("\nHAS GANADO. \nDespués de lanzar el dado %s veces, ganaste un total de $%s",ncaras+nsellos,abs(saldo)))
            victorias = victorias + 1
        }
        i = i + 1 
    } # Imprime el resumen.
    cat(sprintf("\n\nJugamos un total de %s veces.",niter))
    cat(sprintf("\nDe estas, ganaste %s veces, y perdiste %s veces.",victorias,niter-victorias))
    cat(sprintf("\nEn promedio, necesitaste %s lanzamientos para finalizar un juego.",round(prom_lanzamientos,3)))
    if (prom_ganancias<0) cat(sprintf("\n\nPor cada juego, el promedio fue perder $%s",abs(round(prom_ganancias,3)))) else cat(sprintf("\n\nPor cada juego, el promedio fue ganar $%s",round(prom_ganancias,3)))
    if (saldo_global<0) cat(sprintf("\nEn total, perdiste $%s.",abs(saldo_global))) else cat(sprintf("\nEn total, ganaste $%s.",saldo_global))
}

### 4) Desarrollar el ejemplo 2 de la pág 878 del Liberman (Programelo).

### El ejemplo habla de la simulación de un sistema de colas con lambda = 3 por hora y mu = 5 por hora.
### En este caso, se programará esta simulación aceptando un lambda y un mu genéricos.
### También se pide el paso de tiempo, expresado en horas, como el incremento a realizar.

simulacion_cola <- function(lambda, mu){
    
}