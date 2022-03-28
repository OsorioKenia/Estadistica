
#_____________________ MEDIDAS ___________________

# Se trabajar� con la matriz de datos "penguins.xlsx"

#1.-Exportacion de matriz

# Import dataset/from excel/ Browser/ seleccionar
# archivo/ aceptar/ (visualizar)/ import

#2.- Acortar el nombre de la matriz de datos
BD<-penguins

#-----------------------------------------------
# Exploracion de la matriz
#-----------------------------------------------
dim(BD)
str(BD)
colnames(BD)
anyNA(BD)

#-----------------------------------------------
#      Tendencia central
#-----------------------------------------------

# 1.- Media y mediana
summary(BD)


# 2.- Moda

# 2.1.- Se descarga el paquete "modeest"
install.packages("modeest")

# 2.2.- Se abre la librer�a
library(modeest)

# 2.3.- C�lculo de la moda para la variable isla y largo del pico
mfv(BD$isla) # categorica
mfv(BD$largo_pico_mm) # numerica

#-----------------------------------------------
#      Medidas de dispersi�n
#-----------------------------------------------

# 1.- C�lculo de la varianza (s�lo para variables cuantitativas)
var(BD$grosor_pico_mm)

# 2.- C�lculo de la desviaci�n est�ndar
sd(BD$grosor_pico_mm)

# 3.- Error
media_pico<-mean(BD$largo_pico_mm)
media_pico

error<-(BD$largo_pico_mm-(media_pico))
error


#4.- Coeficiente de variacion
CV<- sd(BD$largo_pico_mm)/mean(BD$largo_pico_mm)*100
CV

# 5.- Rango intercuartilico (IQR)
IQR(BD$largo_pico_mm)

# 6.- Rango
pico<-BD$largo_pico_mm
max(pico)
min(pico)

rango<-max(pico)-min(pico)
rango

#-----------------------------------------------
#    Medidas de posici�n
#------------------------------------------------

# 1.- Cuartiles
summary(BD)

# 2.- Quintil
quintil<-quantile(BD[["largo_aleta_mm"]], 
                  p=c(.20, .40, .60, .80))
quintil

# 3.- Decil
decil<-quantile(BD[["largo_aleta_mm"]], 
                p=c(.10, .20, .30, .40, .50, .60,
                    .70, .80, .90))
decil

# Percentil
percentil<-quantile(BD[["largo_aleta_mm"]], 
                    p=c(.33, .66, .99))
percentil

# Interpretacion:
# <192 = Bajo
# 192-209 = Intermedio
# > 209 = Alto

#----------------------------------------------
# Ejercicio 1
#----------------------------------------------

 
# 1.- Media y mediana
summary(BD)


# 2.- Moda

# 2.1.- Se descarga el paquete "modeest"
install.packages("modeest")

# 2.2.- Se abre la librer�a
library(modeest)

# 2.3.- C�lculo de la moda para la variable especie y masa corporal
mfv(BD$especie) # categorica
mfv(BD$masa_corporal_g) # numerica
# El valor de la moda para la variable especie es [1] "Adelie"

#-----------------------------------------------
#      Medidas de dispersi�n
#-----------------------------------------------

# 1.- C�lculo de la varianza (s�lo para variables cuantitativas)
var(BD$masa_corporal_g)
# El valor de la varianza es [1] 641436.2

# 2.- C�lculo de la desviaci�n est�ndar
sd(BD$masa_corporal_g)

# 3.- Error
media_masa<-mean(BD$masa_corporal_g)
media_masa

error<-(BD$masa_corporal_g-(media_masa))
error

#4.- Coeficiente de variacion
CV<- sd(BD$masa_corporal_g)/mean(BD$masa_corporal_g)*100
CV

# 5.- Rango intercuartilico (IQR)
IQR(BD$masa_corporal_g)

# 6.- Rango
masa<-BD$masa_corporal_g
max(masa)
min(masa)

rango<-max(masa)-min(masa)
rango

#-----------------------------------------------
#    Medidas de posici�n
#------------------------------------------------

# 1.- Cuartiles
summary(BD)

# 2.- Quintil
quintil<-quantile(BD[["masa_corporal_g"]], 
                  p=c(.20, .40, .60, .80))
quintil

# 3.- Decil
decil<-quantile(BD[["masa_corporal_g"]], 
                p=c(.10, .20, .30, .40, .50, .60,
                    .70, .80, .90))
decil

# Percentil
percentil<-quantile(BD[["masa_corporal_g"]], 
                    p=c(.33, .66, .99))
percentil
