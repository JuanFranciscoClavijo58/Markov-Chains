##################################
## EJERCICIO CADENAS DE MARKOV ###
##################################

#Primero vamos a crear los estados
library(markovchain)
library(diagram)
library(shape)
library(igra)
statesNames= c("F","N","D")

matriz <- matrix(c(0.96, 0.03, 0.01,  
                   0.24, 0.73, 0.03,  
                   0,    0,    1),    
                 nrow = 3, byrow = TRUE, dimnames = list(statesNames, statesNames))
#Recuerde que la suma tiene que dar 1 (por eso es importante que aparezcan los bucles)

estados <- c("F", "N", "D")

P <- matrix(c(0.96, 0.03, 0.01, 
              0.24, 0.73, 0.03, 
              0,    0,    1), 
            nrow = 3, byrow = TRUE, dimnames = list(estados, estados))

# Matriz de colores (debe tener la misma estructura que P para que tenga sentido
# lo que estoy haciendo)
# El orden es: F->F, F->N, F->D / N->F, N->N, N->D / D->F, D->N, D->D
C <- matrix(c("orange", "darkcyan", "darkred", 
              "royalblue", "deeppink", "forestgreen", 
              "white", "white", "peru"), 
            nrow = 3, byrow = TRUE)

# Gráfico Limpio
# Usamos t(P) y t(C) porque la función lee por columnas
plotmat(t(P), 
        pos = c(2, 1),             # 2 estados arriba y 1 abajo
        name = estados, 
        lcol = t(C),               # Colores de las flechas
        box.col = "white",         # Fondo de los círculos no los queremo rellenos
        box.size = 0.06, 
        box.type = "circle",       # La figura se puede cambiar pero los cirulos siempre son lo mejor
        box.prop = 1,              # Círculos perfectos
        arr.length = 0.3, 
        arr.width = 0.2,
        self.cex = 0.8,            # Tamaño números en bucles (esto lo hago porque los bucles no me aparecian al inico)
        self.shifty = 0.07,        # Desplazamiento vertical de bucles
        self.shiftx = 0.07,        # Desplazamiento horizontal de bucles
        main = "Cadena de Markov: F, N, D")
#####################################################################


