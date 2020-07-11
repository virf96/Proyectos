## Librerías que necesitamos:
library(lubridate) 
library(tseries)
library(forecast)




### Datos faltantes y Outliers 
## Import ts.Rmissing.csv
mydata=read.csv('Rmissing.csv')

# Convertir la segunda columna en una serie de tiempo sin especificar frecuencia
myts = ts(mydata$mydata)
myts

# Comprobar si hay NAs y outliers
summary(myts)
plot(myts)

# Usando zoo para localizar y rellenar valores faltantes
library(zoo)
myts.NAlocf = na.locf(myts) #LOCF: last observation carried forward (copia la última observacion antes del NA)
autoplot(myts.NAlocf ) + ggtitle("Autoplot of Nottingham temperature data")

myts.NAfill = na.fill(myts, 33) #rellena con el valor que le pongamos
autoplot(myts.NAfill) + ggtitle("Autoplot of Nottingham temperature data")

# Detección automática de outliers con la librería forecast
library(forecast)
myts1 = tsoutliers(myts)
myts1 #Estimación de cuanto debería valer la posición que indica
plot(myts)

# También hay un método para NA en el paquete forecast
myts.NAinterp = na.interp(myts) #rellena NA con interpolación
autoplot(myts.NAinterp) + ggtitle("Autoplot of Nottingham temperature data")

# Limpiando NAs y outliers con tsclean del paquete forecast 
mytsclean = tsclean(myts) #Identifica y Sustituye los atípicos por observaciones que deberían ser las correctas
plot(mytsclean)
autoplot(mytsclean) + ggtitle("Autoplot of Nottingham temperature data")
summary(mytsclean)

#------------------------------------------------------------------------------------------------------

#Importando datos de precios de cierre de Starbucks y Microsoft
library(readr)
sbux.df <- read_csv("sbuxPrices.csv")
View(sbux.df)

#Con start indicamos el año y mes en particular, mismo caso con end
#frequency=12 dado que los datos son mensuales, por defecto la opción es 1, es decir asume que son anuales
sbux.ts = ts(data=sbux.df$Adj.Close, frequency = 12,
             start=c(1993,3), end=c(2008,3))
sbux.ts
class(sbux.ts) 


msft.df <- read_csv("msftPrices.csv")
View(sbux.df)
msft.ts = ts(data=msft.df$Adj.Close, frequency = 12,
             start=c(1993,3), end=c(2008,3))
msft.ts
class(msft.ts)

#Fechas y frecuencia de la serie
sbux.ts  
start(sbux.ts) #En que fecha inicia 
end(sbux.ts)  #En que fecha termina
frequency(sbux.ts) #Frecuencia de la serie

#Subconjunto de la serie de tiempo
tmp = sbux.ts[1:5] #Se pierde la información temporal es decir las fechas y ya no es objeto serie de tiempo 
tmp
class(tmp)

#Para tomar intervalos de tiempo lo correcto es usar la función window
tmp = window(sbux.ts, start=c(1993, 3), end=c(1993,8))
tmp
class(tmp)


#Combinando dos series (dos columnas)
sbuxmsft.ts = cbind(sbux.ts, msft.ts) 
sbuxmsft.ts
class(sbuxmsft.ts) 
#mts: multiple time series

#Seleccionando las primeras 5 filas:
window(sbuxmsft.ts, start=c(1993, 3), end=c(1993,7)) 


#Plot objeto ts
plot(sbux.ts, col="blue", lwd=2, ylab="Adjusted close",
     main="Monthly closing price of SBUX") 


#Dibujar un subconjunto (Acercar)
plot(window(sbux.ts, start=c(2000,3), end=c(2008,3)),
     ylab="Adjusted close",col="blue", lwd=2,
     main="Monthly closing price of SBUX") 



#Plot para múltiples columnas
#En gráficos diferentes
plot(sbuxmsft.ts) 

#En el mismo gráfico
plot(sbuxmsft.ts, plot.type="single",
     main="Monthly closing prices on SBUX and MSFT",
     ylab="Adjusted close price",
     col=c("blue", "red"), lty=1:2)
legend(1994, 35, legend=c("SBUX","MSFT"), col=c("blue", "red"),
       lty=1:2)


################### zoo
#La librería zoo permite más flexibilidad en los datos espaciados para las series temporales 
library(zoo) 

#Fecha
#secuencia desde el 1 de marzo de 1993 hasta 1 de marzo del 2008, con frecuencia mensual
td = seq(as.Date("1993/3/1"), as.Date("2008/3/1"), "months") 
class(td) 
head(td) 


#Alternativa
#Utilizamos la variable "date" y la convertimos a fecha indicandole en formato
td2 = as.Date(sbux.df$Date, format="%m/%d/%Y") 
head(td2) 


#Combinando el índice de tiempo a las dos series de precios
#Con la función zoo, le pasamos los valores númericos y los ordemas con el intervalo de fechas que ya creamos
sbux.z = zoo(x=sbux.df$Adj.Close, order.by=td) 
msft.z = zoo(x=msft.df$Adj.Close, order.by=td) 

class(sbux.z) 
str(sbux.z) 
head(sbux.z) 

#Extrayendo el indice de tiempo y los datos
index(sbux.z) 
coredata(sbux.z) 

#Start and End
start(sbux.z) 
end(sbux.z)

#Ventaja de zoo: extraer subconjunto indexando con las fechas
sbux.z[as.Date(c("2000/3/1", "2003/3/1"))]

#window() también funciona
window(sbux.z, start=as.Date("2000/3/1"), end=as.Date("2003/3/1")) 

#Combinando dos series
sbuxmsft.z = cbind(sbux.z, msft.z) 
class(sbuxmsft.z) 
head(sbuxmsft.z) 


#Plot
plot(sbux.z, col="blue", lty=1, lwd=2, ylim=c(0,50),main="Monthly closing prices of SBUX and MFST",
     ylab="Adjusted close price")
lines(msft.z, col="red", lty=2, lwd=2)
legend(x="topleft", legend=c("SBUX","MSFT"), col=c("blue","red"),
       lty=1:2)

#Alternativa, las dos a la vez
plot(sbuxmsft.z, plot.type="single", col=c("blue","red"), lty=1:2,
     lwd=2,main="Monthly closing prices of SBUX and MFST",
     ylab="Adjusted close price")
legend(x="topleft", legend=c("SBUX","MSFT"), col=c("blue","red"),
       lty=1:2) 



#Importar datos directamente como objeto zoo
sbux.z2 = read.zoo("sbuxPrices.csv",
                   format="%m/%d/%Y", sep=",", header=T) 


#Importar datos de Yahoo Finance
library(tseries)
SBUX.z = get.hist.quote(instrument="sbux", start="1993-03-01",
                        end="2020-06-01", quote="AdjClose",
                        provider="yahoo", origin="1970-01-01",
                        compression="d", retclass="zoo") 

View(SBUX.z)

MSFT.z = get.hist.quote(instrument="msft", start="1993-03-01",
                        end="2020-06-01", quote="AdjClose",
                        provider="yahoo", origin="1970-01-01",
                        compression="d", retclass="zoo") 

#Plot
plot(cbind(SBUX.z,MSFT.z), plot.type="single", col=c("blue","red"), lty=1:2,
     lwd=2,main="Monthly closing prices of SBUX and MFST",
     ylab="Adjusted close price")
legend(x="topleft", legend=c("SBUX","MSFT"), col=c("blue","red"),
       lty=1:2)



###################################################################

#Libería dygraphs
install.packages("dygraphs")
library(dygraphs)
dygraph(SBUX.z, "Monthly closing prices of SBUX")
dygraph(cbind(SBUX.z,MSFT.z), "Monthly closing prices of SBUX and MFST")


#############################################################3 Datos diarios 
#Generamos datos aleatorios
datos <- rnorm(78, 0, 10)
fechas <- seq(as.Date("2020-03-06"), as.Date("2020-05-22"), by = "day")
as.numeric(format(fechas[1], "%j")) #Que día del año corresponde la fecha donde inicia el periodo de observación

miserie.ts<-ts(datos,start=c(2016,66), frequency=365)
plot(miserie.ts)


library(zoo)
miserie.z=zoo(datos, fechas)
plot(miserie.z)
dygraph(miserie.z)
