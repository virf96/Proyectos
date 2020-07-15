### Estacionariedad
library(tseries)
library(forecast)

#Serie estacionaria
x <- rnorm(1000) 

#Obtenemos valor el estadistico de contraste, recordemos que el estadistico de contraste debe ser menor al lambda crítico para no rechazar que la serie sea estacionaria
#Obtenemos el número de retrasos con los que esta correlacionado la serie
#El p-value que indica la significancia, en este caso si el p-value es menos a .05 no se rechaza que la serie sea estacionaria
adf.test(x) # Dickey Fuller Test


# Nottem dataset
# Serie de tiempo que contiene el promedio de temperaturas en el castillo de Nottingham en grados Fahrenheit durante 20 años.
plot(nottem)

#Descomposición en efecto de tendencia, estacional y residual.
plot(decompose(nottem))

#Dickey Fuller Test
adf.test(nottem)


#Serie no estacionaria
y <- diffinv(x) #calculamos la inversa de las diferencias de los elementos de x
plot(y)
adf.test(y)


### ACF and PACF (lag.max=numero maximo de retrasos, plot = F si no se quiere dibujar)
#Autocorrelación del df NOTTEM, contiene datos de temperaturas

acf(nottem, lag.max = 20, plot = T)

pacf(nottem, lag.max =20, plot = T)

### ACF of white noise
acf(x, plot = T)



### Otra forma de hacer la descomposición

autoplot(decompose(nottem, type = "additive"))

# Otra aleternativa Seasonal Decomposition of Time Series by Loess
plot(stl(nottem, s.window="periodic")) #s.window: seasonal window

# Extraer los componentes de una serie temporal
mynottem = decompose(nottem, "additive")

class(mynottem)
