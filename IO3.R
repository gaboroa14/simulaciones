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
        if (prom_lanzamientos = 0) prom_lanzamientos = ncaras + nsellos else prom_lanzamientos = (prom_lanzamientos + ncaras + nsellos) / 2 # Promedio de lanzamientos
        if (prom_ganancias = 0) prom_ganancias = saldo else prom_ganancias = (prom_ganancias + saldo) / 2 # Promedio de ganancias
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
### Se programan dos funciones: una simulación con tiempo fijo y otra simulación por eventos.

### POR TIEMPO FIJO:
### Además de Lambda y Mu, también se pide el paso de tiempo, expresado en horas, 
### como el incremento a realizar, y el número de iteraciones a realizar.
### Lambda, Mu y T deben estar expresados en la misma unidad de tiempo.

simulacion_cola_tiempo_fijo <- function(lambda, mu, t, niter){
    p_e = pexp(t,lambda) # Probabilidad de que haya una llegada en el periodo de tiempo t.
    p_s = pexp(t,mu) # Probabilidad de que haya una salida en el periodo de tiempo t.
    i = 0 # Contador de pasos.
    n = 0 # Cantidad de personas que están actualmente en el sistema.
    l = c() # Vector para almacenar el turno en que llega cada uno de los clientes.
    e = 0 # Tiempo de promedio de espera de cada cliente.
    while (i<niter){
        primero = FALSE # Determinaré si esta persona es la primera en llegar al sistema (no puede salir al mismo tiempo que llegó)
        p = abs(runif(1, min=0, max=1)) # Número aleatorio entre 0 y 1.
        cat(sprintf("\n\nSimulación en ejecución. Este es el paso número %s.\nActualmente hay %s clientes en el sistema.",i,n))
        if (p<p_e) {
            if (n==0) primero = TRUE # Si es el primero en llegar, entonces no puede salir este mismo turno.
            n = n + 1
            l = append(l,i) # Agrega el paso en que llegó al vector de llegadas.
            cat(sprintf("\nNueva llegada en el sistema. Actualmente hay %s clientes en el sistema.",n))
        }
        if ((n != 0) && !primero) { # Sólo puede haber una llegada si el sistema no está vacío.
            p = abs(runif(1, min=0, max=1))
            if (p<p_s){
                n = n - 1
                cat(sprintf("\nNueva salida en el sistema. Actualmente hay %s clientes en el sistema.",n))
                x = l[1] # Extraigo en qué turno llegó el primer cliente en cola
                l = l[-c(1)] # Lo elimino del vector (ya fue atendido)
                if (e==0) e = i - x + 1 else e = (e + i - x + 1) / 2
            }
        }
        i = i + 1
    }
    cat(sprintf("\n\nHa terminado la simulación luego de %s iteraciones. \nQuedaron un total de %s clientes en el sistema. \nEl tiempo de espera promedio es de %s unidades de tiempo.",niter,n,round(e*t,2)))
}

### POR EVENTO:
### En este caso, en lugar de trabajar con un incremento fijo, se trabaja por los eventos.
### Cada iteración es en realidad un nuevo evento que sucede.
### Nuevamente, la función toma como parámetros Lambda y Mu y un número total de iteraciones 
### (en este caso eventos) a simular.

simulacion_cola_eventos <- function(lambda, mu, niter){
    i = 0 # Contador de eventos.
    t = 0 # Tiempo total transcurrido.
    n = 0 # Cantidad de clientes en el sistema.
    e_e = 0 # Tiempo esperado de espera.
    c_e = 0 # Cantidad esperada de clientes en el sistema.
    while (i<niter){
        t_l = round(-log(abs(runif(1, min=0, max=1)))/lambda,3) # El tiempo para una nueva llegada.
        if (n!=0) t_s = round(-log(abs(runif(1, min=0, max=1)))/mu,3) # El tiempo para una nueva salida, si hay clientes en el sistema.
        if (n==0){ # Si no hay nadie en el sistema, sólo puede haber una llegada:
            n = n + 1 # Aumenta la cantidad de clientes.
            t = t + t_l # Aumenta el tiempo total transcurrido.
            cat(sprintf("\n\nHan pasado %s unidades de tiempo desde que inició la simulación.\nSe registra una llegada tras %s unidades de tiempo. \nAhora hay %s clientes en sistema.",t,t_l,n))
        } else { # Si hay clientes en el sistema, puede haber tanto llegadas como salidas...
            cat(sprintf("\nTiempo hasta la próxima llegada: %s. \nTiempo hasta la próxima salida: %s.",t_l,t_s))
            if (t_l < t_s){ # Si hay una llegada, igual al caso anterior.
                n = n + 1
                t = t + t_l
                cat(sprintf("\n\nHan pasado %s unidades de tiempo desde que inició la simulación.\nSe registra una llegada tras %s unidades de tiempo. \nAhora hay %s clientes en sistema.",t,t_l,n))
            } else { # Si hay una salida, disminuye la cantidad de clientes en el sistema.
                n = n - 1
                t = t + t_s
                if (e_e == 0) e_e = t_s else e_e = (e_e + t_s) / 2 # Para el tiempo esperado de espera.
                cat(sprintf("\n\nHan pasado %s unidades de tiempo desde que inició la simulación.\nSe registra una salida tras %s unidades de tiempo. \nAhora hay %s clientes en sistema.",t,t_l,n))
            }
        }
        if (c_e == 0) c_e = n else c_e = (c_e + n) / 2 # Para la cantidad esperada de clientes en el sistema.
        i = i + 1
    }
    cat(sprintf("\n\nLa simulación ha terminado tras %s unidades de tiempo. \nQuedaron %s clientes en el sistema. \n\nEl tiempo esperado de espera es de %s.\nLa cantidad esperada de clientes en el sistema es %s.",round(t,3),n,round(e_e,3),round(c_e,3)))
}

### 7) Generación de números aleatorios
### e) Describa un algoritmo para generar números aleatorios.

### Número Aleatorio con el método Congruencial Mixto.
### Vamos a programar un algoritmo que genere un número aleatorio congruencial, basados en el método
### congruencial. Este algoritmo recibe como parámetro un número entero que indica la cantidad de números 
### aleatorios que va a arrojar como resultado. 

### Además de esto, también recibe los siguientes parámetros opcionales:

### Recibe la semilla, o x0, que es el primer número de la secuencia a partir del cual se construyen los
### demás. Por defecto, se trabaja semilla = 1.

### Para los valores de a, c, m, se toman los valores correspondientes al estándar POSIX C, los cuales son:
### c = 12345, m = 32768, a = 1103515245. 
### Sin embargo, también se reciben como parámetros opcionales, por si se quiere probar con otro valores.

### Como este método genera TODA la secuencia de números aleatorios con estos atributos, lo que consume
### mucho tiempo y recursos, también se ofrece un parámetro opcional para determinar la cantidad de 
### elementos de la secuencia a generar como muestra de la cual se tomarán los números. Por defecto, 
### esta cantidad es de 1000.

### Del mismo modo, se ofrece otro parámetro llamado uniforme para determinar si el algoritmo
### arrojará un número aleatorio uniforme o un número aleatorio entero.

### Este método es muy poco eficiente a nivel computacional, ya que en cada ejecución genera la
### secuencia de números a partir de la cual se toman las muestras. Es por ello que esta función
### difícilmente se podrá utilizar en un entorno de producción; teóricamente se utiliza sólo como
### método para entender la generación de números aleatorios congruenciales.

numero_aleatorio <- function(n, semilla = 1, uniforme = FALSE, cant = 1000, c = 12345, m = 32768, a = 1103515245){
    numeros = c(if (!uniforme) semilla else (semilla+0.5)/m) ### Genera un vector donde se guardarán los números aleatorios.
    ejecutar = TRUE ### Para ejecutar el algoritmo.
    while (ejecutar && length(numeros)<cant){ ### Mientras que se vaya a ejecutar y la longitud sea menor que la cantidad,
        x = (a*numeros[length(numeros)]+c) %% m ### Genero nuevo número aleatorio.
        ejecutar = !(x %in% numeros) ### Si el número no está en el vector, puede continuar ejecutando. Caso contrario, cierro ejecución.
        if (ejecutar) numeros=c(numeros,if (!uniforme) x else (x+0.5)/m) ### Si el número no está en el vector, lo almacena en él.
    }
    sample(numeros,n) ### Una vez lleno el vector, toma una muestra de n números de él.
}   

### 11) Programar el método de la transformada inversa con la distribución uniforme.

### En este caso, se trata de una fórmula relativamente trivial.
### Se reciben como parámetros los valores a, b correspondientes a la distribución,
### y el número r al cual se le encontrará la transformación inversa.

transformada_inversa_uniforme <- function(a,b,r){
    x = a + (b - a) * r
    cat(sprintf("La transformada inversa uniforme en este caso tiene como resultado %s.",x))
}

### 15) Programar el método del rechazo y realizar la corrida del mismo con el ejemplo 18 − 36 de la 
### pág 654 del Taha.

