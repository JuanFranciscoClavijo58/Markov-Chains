library(markovchain)
library(diagram)
estados <- c("F", "N", "D")
mc_modelo <- new("markovchain", 
                 states = estados, 
                 transitionMatrix = matrix(c(0.96, 0.03, 0.01, 
                                             0.24, 0.73, 0.03, 
                                             0,    0,    1), 
                                           nrow = 3, byrow = TRUE, 
                                           dimnames = list(estados, estados)))

matriz_extraida <- as(mc_modelo, "matrix")

#Toca transponer la matriz para que plotmat lo lea bien 
plotmat(t(matriz_extraida), 
        pos = c(2, 1), # orden de mis estados dos arriba y uno abajo
        name = estados, 
        box.size = 0.06, 
        box.type = "circle", 
        box.col = "white",
        #para los bucles
        self.shifty = 0.09, 
        self.shiftx = 0.07,
        main = "Grafo markovchain")

# ¿Qué probabilidad hay de estar en saludable y a los dos años enfermo (pasar de F a N)?
prediccion2años <- (mc_modelo ^ 2)
prediccion2años[1,2]
#####################################################################################
# ¿Calcule la probabilidad de que un arbol saludable nunca llegue a estar enfermo? ##
#####################################################################################
# Esta puede ser una manera de convergencia de la serie pero computacionalmente
# n <- 1:500  # Probamos con 500 años para aproximar al infinito porque 
# la esperanza de vida no va a ser mayor a esto
# terminos <- 0.01 * (0.96)^(n-1) 
# suma_acumulada <- sum(terminos)
# suma_acumulada

######################
# Solución Pro #######
######################

# la idea es convertir tambien a N como un estado absorvente 
# y calcular las probabilidades de absorcion y tomar la que correspona a no ir a N
matriz_temp <- as(mc_modelo, "matrix")
matriz_temp["N", ] <- c(0, 1, 0)
mc_temp <- new("markovchain", states = estados, transitionMatrix = matriz_temp)
absorptionProbabilities(mc_temp)

###########################################################
#¿Qué pasa si dejamos que el tiempo pase para siempre? ####
###########################################################

absorbingStates(mc_modelo)










