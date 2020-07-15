### Suavizado con SMA

library("TTR")

# Ejemplo mas sencillo
# n es el orden del SMA

x = c(1,2,3,4,5,6,7) #Generamos un vector


#Realizamos el suavizamiento por promedios móviles, en donde indicamos ventana temporal de 3 datos
SMA(x, n = 3) # 3er order: 2 NA's


# Ejemplo datos linces

#Cargamos el df con datos del número de linces capturados en Cánada

lynxsmoothed = SMA(lynx, n = 3); lynxsmoothed

autoplot(lynx) #Datos reales
autoplot(lynxsmoothed) #Datos con suavizamiento por promedios móviles con ventana temporal de 3 datos


lynxsmoothed = SMA(lynx, n = 9); lynxsmoothed
autoplot(lynx)#Datos reales
autoplot(lynxsmoothed) #Datos con suavizamiento por promedios móviles con ventana temporal de 9 datos





### Suavizado exponencial con la función ets

library(forecast)

# Funcion ets
#Al no indicar ningún parametro el ajuste es de manera automatica
#En el caso de que quisieramos indicar un suavizamiento exponencial simple deberiamos indicar el método de estimación de los errores en el parametro "model=ANN"
#En el caso de que quisieramos indicar un suavizamiento exponencial doble deberiamos considerar la tendencia y estimación de errores en el parametro "model=MAN"
#En el caso de querer indicar un suavizamiento exponencial triple se debe considerar la estacionalidad,tendencia y estimación de errores en parametro "model=MAM"
etsmodel = ets(nottem); etsmodel #Aplicamos el suavizamiento exponencial al df nottem con datos de temperaturas y lo guardarmos en etsmodel


# Modelo vs original
plot(nottem, lwd = 3)
lines(etsmodel$fitted, col = "red") #Observamos que el ajuste es bastante bueno

# Forecast
#Pronosticamos 12 periodos hacía adelante 
#Además de la estimación obtenemos una zona gris que es un intervalo de confianza
plot(forecast(etsmodel, h = 12))

# Intervalo de prediccion: nivel de confianza 95%
#Para estar seguros podemos asignar al parametro level un nivel de confianza para la estimación
plot(forecast(etsmodel, h = 12, level = 95))

# Holt Winters multiplicativo (Suavizamiento Exponencial Triple)
etsmodmult = ets(nottem, model ="MMM"); #error, tendencia, estacionalidad. Defecto model=ZZZ (selecciona automaticamente)

# Comparación
plot(nottem, lwd = 3)
lines(etsmodmult$fitted, col = "red")
