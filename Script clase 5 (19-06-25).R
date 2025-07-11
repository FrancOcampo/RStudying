# << DATOS BASICOS  >> ####

 base::rm(list = base::ls()); grDevices::graphics.off() # BORRA ENVIRONMENT Y PLOTS
 

# PAQUETES ####
# PARA INSTALAR LIBRERIAS UTILIZADAS

# Codigo instalacion:
# install.packages()

# base::library(stats)
# base::library(readxl)
# base::library(tseries)
# base::library(urca)
# base::library(graphics)
# base::library(lmtest)
# base::library(forecast)

## DATOS ####

datos <- readxl::read_excel("C:/Users/lzanini/OneDrive - bcsf.com.ar/Escritorio/DATA.xlsx")
attach(datos)

ICA <- stats::ts(ICASFE, frequency = 12, start = base::c(1996,01))
VSUP <- stats::ts(VSUP, frequency = 12, start = base::c(1996,01))

## EXPLORACION GRAFICA ####

#Grafico la serie y analizo
stats::ts.plot(ICA)
stats::ts.plot(diff(ICA))
ICA.int <- diff(ICA)

stats::ts.plot(VSUP)
stats::ts.plot(diff(VSUP))        
VSUP.int <- diff(VSUP)

#¿como observo estacionalidad?
meses <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
           "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
boxplot(ICA ~ cycle(ICA), main = "Boxplot estacional", 
        xlab = "Mes", ylab = "Valor", names = meses)
forecast::ggseasonplot(ICA)

# no se observa un componente estacional claro

boxplot(ICA.int ~ cycle(ICA.int), main = "Boxplot estacional", 
        xlab = "Mes", ylab = "Valor", names = meses)
forecast::ggseasonplot(ICA.int)

boxplot(VSUP ~ cycle(VSUP), main = "Boxplot estacional", 
        xlab = "Mes", ylab = "Valor", names = meses)
forecast::ggseasonplot(VSUP)
# se ve una estacionalidad marcada en diciembre

boxplot(VSUP.int ~ cycle(VSUP.int), main = "Boxplot estacional", 
        xlab = "Mes", ylab = "Valor", names = meses)
forecast::ggseasonplot(VSUP.int)

## TEST DICKEICA## TEST DICKEY-FULLER AUMENTADO (ADF) ####
## TEST DE RAIZ UNITARIA 
### TEST CON PAQUETE URCA #### 
# ALTERNATIVA MAS CORRECTA DEL TEST
# PERMITE DIFERENTES ESPECIFICACIONES  

# HO = EXISTE RAIZ UNITARIA | SI RECHAZO => ESTACIONARIA 
# PARA RECHAZAR tau1,DEBE SER MENOR A LOS VALORES EXPUESTOS EN 1pct, 5pct, 10pct

adf.ICA <- urca::ur.df(y=stats::na.omit(ICA), type='none')
urca::summary(adf.ICA)
# NO RECHAZO H0 -> NO ESTACIONARIA

adf.dICA <- urca::ur.df(y=stats::na.omit(ICA.int), type='none')
urca::summary(adf.dICA)
# RECHAZO H0 -> ESTACIONARIA

adf.VSUP <- urca::ur.df(y=stats::na.omit(VSUP), type='none')
urca::summary(adf.VSUP)
# NO RECHAZO H0 -> NO ESTACIONARIA

adf.d.VSUP <- urca::ur.df(y=stats::na.omit(VSUP.int), type='none')
urca::summary(adf.d.VSUP)
# RECHAZO H0 -> ESTACIONARIA

## MODELADO ICASFE ####
### FUNCIONES DE AUTOCORRELACION ####

# trabajo directamente sobre las diferencias
graphics::par(mfrow = base::c(1, 2))  # 2 FILAS Y 1 COLUMNA PARA ACF Y PACF EN UN SOLO PANEL

stats::acf(na.omit(ICA.int),lag.max = 60, ylim = c(-1, 1))
stats::pacf(na.omit(ICA.int),lag.max = 60, ylim = c(-1, 1))
# a priori podemos proponer un AR(1), pero hay un componente estacional que hace ruido

#ANALIZO AUTOCORRELOGRAMAS EN LA PARTE ESTACIONAL

ICA_seasonal <- diff(ICA, lag = 12)  # Diferenciación estacional
acf(ICA_seasonal, lag.max = 48, ylim = c(-1, 1))
pacf(ICA_seasonal, lag.max = 48, ylim = c(-1, 1))
# ARIMA(2,0,0) estacional? 

ICA_seasonal_int <- diff(ICA_seasonal)  # d=1 y D=1
acf(ICA_seasonal_int, lag.max = 48, ylim = c(-1, 1))
pacf(ICA_seasonal_int, lag.max = 48, ylim = c(-1, 1))
# podemos proponer un ARIMA(1,1,0) estacional

### MODELOS SARIMA ####
ica.110.110 <- forecast::Arima(ICA, order = c(1, 1, 0),
              seasonal = list(order = c(1, 1, 0)))
summary(ica.110.110)
lmtest::coeftest(ica.110.110)

forecast::checkresiduals(ica.110.110)
# Parece que queda algo en la estacionalidad

#Pruebo modelo SARIMA (110)(200)
ica.110.200 <- forecast::Arima(ICA, order = c(1, 1, 0),
                               seasonal = list(order = c(2, 0, 0)))
summary(ica.110.200)
lmtest::coeftest(ica.110.200)

forecast::checkresiduals(ica.110.200)
# Mejora bastante

acf(ica.110.200$residuals, lag.max = 48, ylim = c(-1, 1))
pacf(ica.110.200$residuals, lag.max = 48, ylim = c(-1, 1))
# hay algo raro en en lag 36

ica.1.stdres <- ica.110.200$residuals/sd(ica.110.200$residuals)
ts.plot(ica.1.stdres, ylab = "Residuos estandarizados", main = "Serie de residuos estandarizados")
abline(h = 0, col = "red")

AIC(ica.110.200)/ica.110.200$nobs
# 1.443
BIC(ica.110.200)/ica.110.200$nobs
# 1.498

# Pruebo modelo SARIMA(110)(112)
ica.110.112 <- forecast::Arima(na.omit(ICA), order = c(1, 1, 0),
                            seasonal = list(order = c(1, 1, 2)))
summary(ica.110.112)
lmtest::coeftest(ica.110.112)

forecast::checkresiduals(ica.110.112)
acf(ica.110.112$residuals, lag.max = 48, ylim = c(-1, 1))
pacf(ica.110.112$residuals, lag.max = 48, ylim = c(-1, 1))

ica.2.stdres <- ica.110.112$residuals/sd(ica.110.112$residuals)
ts.plot(ica.2.stdres, ylab = "Residuos estandarizados", main = "Serie de residuos estandarizados")
abline(h = 0, col = "red")

AIC(ica.110.112)/ica.110.112$nobs
# 1.637
BIC(ica.110.112)/ica.110.112$nobs
# 1.693



# me quedo con el 110 200
# grafico
graphics::par(mfrow = base::c(1, 1))

ICA_fitted <- stats::fitted(ica.110.200)

stats::ts.plot(ICA, ICA_fitted, col = c("black", "red"), lty = c(1, 1), 
               main = "Serie original ICA vs. Ajuste ARIMA(1,1,1)", ylab = "Valor", 
               lwd = 2)
graphics::legend("topleft", 
                 legend = c("Serie original", "Ajuste ARIMA(1,1,1)"), 
                 col = c("black", "red"), 
                 lty = c(1, 1), 
                 lwd = 2)

## MODELO VSUP ####
### FUNCIONES DE AUTOCORRELACION ####
stats::acf(na.omit(diff(log(VSUP))),lag.max = 60, ylim = c(-1, 1))
stats::pacf(na.omit(diff(log(VSUP))),lag.max = 60, ylim = c(-1, 1))
# a priori podríamos proponer un ARIMA(4,1,1) pero hay mucha estacionalidad
# puede ser solo AIRMA (0,1,1)

#ANALIZO AUTOCORRELOGRAMAS EN LA PARTE ESTACIONAL

VSUP_seasonal <- diff(log(VSUP), lag = 12)  # Diferenciación estacional
acf(VSUP_seasonal, lag.max = 48, ylim = c(-1, 1))
pacf(VSUP_seasonal, lag.max = 48, ylim = c(-1, 1))
#podríamos proponer un AR(3) pero tomemos una diferencia xq no parece estacionario

VSUP_seasonal_int <- diff(VSUP_seasonal)  # d=1 y D=1
acf(VSUP_seasonal_int, lag.max = 48, ylim = c(-1, 1))
pacf(VSUP_seasonal_int, lag.max = 48, ylim = c(-1, 1))
# proponemos un ARIMA(2,1,1) para la parte estacional

### MODELOS SARIMA ####
vsup.011.211 <- forecast::Arima(log(VSUP), order = c(0, 1, 1),
                               seasonal = list(order = c(2, 1, 1)))
summary(vsup.011.211)
lmtest::coeftest(vsup.011.211)

forecast::checkresiduals(vsup.011.211)
# no parece ruido blanco

# Pruebo 
vsup.311.211 <- forecast::Arima(log(VSUP), order = c(3, 1, 1),
                                seasonal = list(order = c(2, 1, 1)))
summary(vsup.311.211)
lmtest::coeftest(vsup.311.211)

# AR 2 y 3 no significativos

#saco ar3
vsup.211.211 <- forecast::Arima(log(VSUP), order = c(2, 1, 1),
                                seasonal = list(order = c(2, 1, 1)))
summary(vsup.211.211)
lmtest::coeftest(vsup.211.211)

forecast::checkresiduals(vsup.211.211)

acf(vsup.211.211$residuals, lag.max = 48, ylim = c(-1, 1))
pacf(vsup.211.211$residuals, lag.max = 48, ylim = c(-1, 1))
# no logro ruido blanco

#pruebo 211.212
vsup.211.212 <- forecast::Arima(log(VSUP), order = c(2, 1, 1),
                                seasonal = list(order = c(2, 1, 2)))
summary(vsup.211.212)
lmtest::coeftest(vsup.211.212)

forecast::checkresiduals(vsup.211.212)

acf(vsup.211.212$residuals, lag.max = 48, ylim = c(-1, 1))
pacf(vsup.211.212$residuals, lag.max = 48, ylim = c(-1, 1))

#pruebo 211.300 (AR 3 en el seasonal)
vsup.211.300 <- forecast::Arima(log(VSUP), order = c(2, 1, 1),
                                seasonal = list(order = c(3, 0, 0)))
summary(vsup.211.300)
lmtest::coeftest(vsup.211.300)

forecast::checkresiduals(vsup.211.300)

acf(vsup.211.300$residuals, lag.max = 48, ylim = c(-1, 1))
pacf(vsup.211.300$residuals, lag.max = 48, ylim = c(-1, 1))

#pruebo 211.301
vsup.211.301 <- forecast::Arima(log(VSUP), order = c(2, 1, 1),
                                seasonal = list(order = c(3, 0, 1)))
summary(vsup.211.301)
lmtest::coeftest(vsup.211.301)

forecast::checkresiduals(vsup.211.301)
