library(tidyverse)
library(lubridate)
library(quantmod)
library(xts)
library(tseries)
library(zoo)
library(reshape)
library(readr)
library(dplyr)
library(forecast)
library(seasonal)
library(tsibble)
library(urca)
library(uroot)
library(gridExtra)
library(Metrics)
library(scales)


##install.packages("forecast",dependency=TRUE)

#Descargando la data sobre covid mundial
data <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")

#Filtrando los datos de Mexico
data <- data[data$location=="Mexico",]
#view(data)

#Filtrando datos de interes:
covid_mx <- select(data, date, new_deaths)
covid_mx <- covid_mx[1:880,] ## Limita la fecha hasta el día 30/may/22
#date =columna de fecha desde 01/01/2020 hasta hoy con frecuencia diaria
#new_deaths = número de muerte diarias por covid-19 en México
#new_cases  =  número de casos positivos nuevos de covid-19 en México
#people_vaccinated =número de personas vacunas con al menos una dosis o más 
# contra el covid
view(covid_mx)

#Limpiando valores nulos

print("Número de valores nulos:")
print(sum(is.na(covid_mx$new_deaths)))

i=0;
for (i in 1:nrow(covid_mx)){
  for (name in colnames(covid_mx)){
    if (i==1 & name!="date"){
      covid_mx[1,name]=0;
    }else{
      if(is.na(covid_mx[i,name])==TRUE  & is.na(covid_mx[i+1,name]==TRUE) ){
        covid_mx[i,name]=covid_mx[i-1,name]
      }
      if(is.na(covid_mx[i,name]==TRUE)  & is.na(covid_mx[i+1,name])==FALSE ){
        covid_mx[i,name]= round(((covid_mx[i-1,name])+(covid_mx[i+1,name]))/2)
      }
      
    }
    
  }
}

######## Cambiando tiempo de muestreo de diario a semanal ################

covid_mx <- covid_mx %>%
  mutate(week=tsibble::yearweek(covid_mx$date))

covid_week <- covid_mx %>%
  group_by(week) %>%
  summarise(new_deaths=round(mean(new_deaths)))

covid_week <- covid_week %>%
  mutate(date=seq(as.Date(covid_mx$date[1]),as.Date(Sys.Date()),by="week"))

## Identificadores de año y mes

covid_week<-covid_week %>% 
  mutate(anio=format(date, "%Y"),
         mes_tex=format(date,"%b"))

### Analisis exploratorio de los datos ###
  
# Graficando datos nuevos fallecimientos:
ggplot(data=covid_week, aes(x=date  , y=new_deaths))+
  geom_line(colour="black")+
  scale_x_date(date_breaks = "6 month",
               date_labels = " %m-%Y")+
  scale_y_continuous(breaks = seq(min(covid_week$new_deaths),
                                 max(covid_week$new_deaths),(100)))+
  geom_area(alpha=0.6)+
  theme_minimal()+
  labs(title="Nuevos fallecimentos semanales por COVID-19",
       subtitle = "en Mexico del años 2020 al 2022",
       caption = "Elaboración propia con con datos de Our World in Data COVID-19",
       x="Fecha",
       y="Número de fallecimientos")


### Estadisticos por año
covid_anio<-covid_week %>% 
  group_by(anio) %>% 
  summarise(min_anual=min(new_deaths),
            max_anual=max(new_deaths),
            prom_anual=mean(new_deaths),
            std_anual=sd(new_deaths))
            
covid_anio

### Estadisticos por mes
covid_mensual<-covid_week %>% 
  group_by(mes_tex) %>% 
  summarise(min_anual=min(new_deaths),
            max_anual=max(new_deaths),
            prom_anual=mean(new_deaths),
            std_anual=sd(new_deaths))

covid_mensual


#box plot
##covid_week %>% 
  ggplot()+
  geom_boxplot(data=covid_week,aes(x=fct_relevel(mes_tex,"ene.","feb.","mar.","abr.","may.","jun.","jul.","ago.","sep.","oct.","nov.","dic."), y=new_deaths), fill="steelblue", color="black",
               outlier.colour = "red")+
  theme_minimal()+
  labs(title = "Box-plot fallecimientos semanales por COVID-19",
       subtitle = "en Mexico del años 2020 al 2022",
       caption = "Elaboración propia con con datos de Our World in Data COVID-19",
       x="Fecha",
       y="Número de fallecimientos")


covid_week %>% 
  group_by(anio) %>% 
  ggplot(aes(x=anio, y=new_deaths))+
  geom_boxplot(fill="steelblue", color="black",
               outlier.colour = "red")+
  theme_minimal()+
  labs(title = "Box-plot fallecimientos semanales por COVID-19",
       subtitle = "en Mexico del años 2020 al 2022",
       caption = "Elaboración propia con con datos de Our World in Data COVID-19",
       x="Fecha",
       y="Número de fallecimientos")


#Histograma

library(statip)
library(modeest)

ggplot(covid_week, aes(new_deaths))+
  geom_histogram(col="black", fill="red", alpha=0.4,
                 breaks=pretty(range(covid_week$new_deaths),
                               nclass.Sturges(covid_week$new_deaths),
                               min.n = 1))+
  theme_minimal()+
  geom_vline(xintercept = mean(covid_week$new_deaths),
             color="black", linetype="dashed")+
  geom_vline(xintercept = median(covid_week$new_deaths),
             color="brown", linetype="dashed")+
  geom_vline(xintercept = mfv(covid_week$new_deaths),
             color="steelblue", linetype="dashed")+
  labs(title = "Histograma fallecimientos semanales \npor Covid-19 del 2020 al 2022 en México",
       
       x="Clases",
       y="Frecuencia")


## Q-Q plot
ggplot(covid_week, aes(sample=new_deaths))+
  stat_qq(color="#2E86C1")+
  stat_qq_line(color="#C0392B", size=0.7)+
  theme_minimal()+
  labs(title = "Gráfico Q-Q fallecimientos semanales \npor Covid-19 del 2020 al 2022 en México")



## Aplicación de una prueba jarque-bera 

jarque.bera.test(covid_week$new_deaths)

# Dado el p-valor del estadistico jarque-bera, y en contraste con
# un valor de significacia del 0.05, hay evidencia para rechazar
# H0=distribución normal, por lo que se asume que la serie de tiempo no
# sigue una distribución normal

## NOTA: La serie de tiempo naturalmente
## no presentará una distribución normal


## Conviertiendo los datos en series de tiempo  
ts_newdeaths<-ts(data=covid_week$new_deaths,
                 start = c(2020,1),
                 frequency = 52)


#Descomposición temporal

des_new_deaths<-decompose(ts_newdeaths)

plot(des_new_deaths)

# Analsis de la estacionariedad serie de tiempo a niveles I(0):
#1. Funciones de autocorrelación simple.

ACF<-Acf(ts_newdeaths, lag.max=50, plot = F) %>% 
  autoplot()+
  labs(title="A)",
       subtitle = 'Función de correlación simple fallecimientos semanales\npor Covid-19 en México a niveles I(0)',
       y=expression(rho),
       x='k')+
  scale_x_continuous(breaks = seq(0,50,5))
#1.1 Funciones de autocorrelación parcial

PACF<-Pacf(ts_newdeaths, lag.max=156, plot = F) %>% 
  autoplot()+
  labs(title="B)",
       subtitle = 'Función de correlación parcial fallecimientos semanales\npor Covid-19 en México a niveles I(0)',
       y=expression(rho),
       x='k')+
  scale_x_continuous(breaks = seq(0,156,52))
  
ACF_PACF <- grid.arrange(ACF, PACF)
plot(ACF_PACF)


#2. Prueba de Dickey-Fuller Aumentada.

adf.test(ts_newdeaths)

# Dado que el estadistico Dickey-Fuller Aumentada proporcionó un p-value=0.2317 
# y en contraste con un valor de significancia alpha=0.05, no existe evidencia 
# suficiente para rechazar la hipostesis Ho de no estacionariedad.



#3. Prueba de Phillips-Perron.
pp.test(ts_newdeaths)

# Dado que el estadistico Phillips-Perron proporcionó un p-value=0.5503 
# y en contraste con un valor de significancia alpha=0.05, no existe evidencia 
# suficiente para rechazar la hipostesis Ho de no estacionariedad.


#4. Prueba de KPSS.

kpss.test(ts_newdeaths, null = "Trend")
# Dado que el estadistico KPSS proporcionó un p-value=0.01
# y en contraste con un valor de significancia alpha=0.05, existe evidencia 
# suficiente para rechazar la hipostesis Ho de estacionariedad.

  
# Considerando los estadisitcos Dickey-Fuller Aumentada,Phillips-Perron y KPSS
# se confirma la no estacionariedad de la serie de tiempo, porque para cumplir
# con el supuesto de ruido blanco y varianza cte, se procede a realizar la 1ra
# diferencia de la serie de tiempo, para posterior volver a verificar la 
# estacionariedad de la serie de tiempo en primera diferecia I(1)

ts_dif_newdeaths <- diff(ts_newdeaths)

dif_newdeaths <- data.frame(date=covid_week$date[2:nrow(covid_week)],
                            ts_dif_newdeaths)

ggplot(data=dif_newdeaths, aes(x=date, y=ts_dif_newdeaths))+
  geom_line(colour="black")+
  scale_x_date(date_breaks = "6 month",
               date_labels = " %m-%Y")+
  theme_minimal()+
  labs(title="Nuevos fallecimentos semanales por COVID-19\ndel 2020 al 2022 en México",
       subtitle = "Serie de tiempo con primera diferencia I(1)",
       caption = "Elaboración propia con con datos de Our World in Data COVID-19",
       x="Fecha",
       y="Número de fallecimientos")


# Analsis de la estacionariedad serie de tiempo en primer diferencia I(1):
#1. Funciones de autocorrelación simple.

ACF<-Acf(ts_dif_newdeaths, lag.max=50, plot = F) %>% 
  autoplot()+
  labs(title= 'A)',
       subtitle = 'Función de correlación simple fallecimientos semanales\npor Covid-19 en México I(1)',
       y=expression(rho),
       x='k')+
  scale_x_continuous(breaks = seq(0,50,5))

# Se sospecha de estacionariedad ya que existe un caida exponencial en los  
# primeros retardos. Sin embargo, es necesario aplicar las pruebas de estacio-
# nariedad KPSS, DF y PP 

#1.1 Funciones de autocorrelación parcial

PACF<-Pacf(ts_dif_newdeaths, lag.max=156, plot = F) %>% 
  autoplot()+
  labs(title= 'B)',
       subtitle = 'Función de correlación simple fallecimientos semanales\npor Covid-19 en México I(1)',
       y=expression(rho),
       x='k')+
  scale_x_continuous(breaks = seq(0,156,52))

ACF_PACF <- grid.arrange(ACF, PACF)
plot(ACF_PACF)


# Se sospecha de estacionalidad ya que existe una concordancia en el signo de
# rho para los retardos 1,52 y 104. Sin embargo, es necesario aplicar la 
# prueba de DF estacional 

#2. Prueba de Dickey-Fuller Aumentada.

adf.test(ts_dif_newdeaths)

# Dado que el estadistico Dickey-Fuller Aumentada proporcionó un p-value=0.01 
# y en contraste con un valor de significancia alpha=0.05, existe evidencia 
# suficiente para rechazar la hipostesis Ho de no estacionariedad.


#3. Prueba de Phillips-Perron.
pp.test(ts_dif_newdeaths)

# Dado que el estadistico Phillips-Perron proporcionó un p-value=0.01 
# y en contraste con un valor de significancia alpha=0.05, existe evidencia 
# suficiente para rechazar la hipostesis Ho de no estacionariedad.


#4. Prueba de KPSS.

kpss.test(ts_dif_newdeaths, null = "Level")
# Dado que el estadistico KPSS proporcionó un p-value=0.1
# y en contraste con un valor de significancia alpha=0.05,no existe evidencia 
# suficiente para rechazar la hipostesis Ho de estacionariedad.

# Considerando los estadisitcos Dickey-Fuller Aumentada,Phillips-Perron y KPSS
# se confirma la estacionariedad de la serie de tiempo en primera diferencia,
# es decir, que la serie de tiempo posee media igual con cero y varianza cte.
# Por lo tanto, se procede a realizar un analisis de la estacionalidad de la 
# serie de tiempo en diferencia I(1), para posterior tomar una desición sobre
# el modelado basado en ARIMA o SARIMA segun sea el caso.


############## Estacionalidad ##########

ggseasonplot(ts_newdeaths, year.labels = TRUE,
             year.labels.left = TRUE,
             main='Analisis de estacionalidad nuevos fallecimientos COVID-19 
             en México')

ggseasonplot(ts_dif_newdeaths, year.labels = TRUE,
             year.labels.left = TRUE,
             main='Analisis de estacionalidad nuevos fallecimientos
             por COVID-19 en México')


#En el grafico se aprecia que el año 2020 no tiene la misma temporalidad 
#respecto a los años 2021 y 2022, por otra parte, el año 2022, parece seguir la
#misma tendencia que el año 2021 para los primeros meses de años. Sin embargo,
#antes de confirmar la estacionalidad en la serie de tiempo, es necesario 
#aplicar la prueba Dickey-Fuller estacional.

## Prueba DF estacional 

covid_s <- diff(ts_newdeaths,52)

## diff() -> Obtiene la diferencia de una serie
## diff(x,1) -> Y_t - Y_{t-1}
## diff(x,4) -> Y_t - Y_{t-4}

covid_t4 <- stats::lag(ts_newdeaths,-52)

## stats::lag() obtiene el rezago de la serie 
## stats::lag(x,-1) -> Y_{t-1}
## stats::lag(x,-4) -> Y_{t-4}

#Para Dickey-Fuller estacional
#H0= estacional
#Ha= no estacional
DFS <- lm(covid_s~covid_t4[1:74])
summary(DFS)
#Dado que el p-valor del phi estimado es 2x10^-16 y constrastado con un valor
#de significia del 0.05, existe evidencia suficiente para rechazar H0, es decir
#que el valor de phi es != 0. y por lo tanto no hay presencia de raiz no 
#estacional, concluyendose que la serie no tiene presencia estacional.

#Dado que no existe evidencia de estacionalidad se procede a realizar el 
#modelado mediante ARIMA

###################### modelo ARIMA  #################################

#Seleccion de los ordenes del modelo ARIMA q
#Función de autocorrelación simple
ACF<-Acf(ts_dif_newdeaths, lag.max=52, plot = F) %>% 
  autoplot()+
  labs(title= 'Funcion de correlación simple',
       subtitle = 'Serie de tiempo fallecimientos semanales por Covid-19 en
       México con 1ra diferencia I(1)',
       y=expression(rho),
       x='k')+
  scale_x_continuous(breaks = seq(0,52,1))

# q = 1

#Seleccion de los ordenes del modelo ARIMA q
#Función de autocorrelación parcial

PACF<-Pacf(ts_dif_newdeaths, lag.max=52, plot = F) %>% 
  autoplot()+
  labs(title= 'Funcion de correlación parcial',
       subtitle = 'Serie de tiempo fallecimientos semanales por Covid-19 en 
       México con 1ra diferencia I(1)',
       y=expression(rho),
       x='k')+
  scale_x_continuous(breaks = seq(0,52,1))

# p = 2,4

ACF_PACF <- grid.arrange(ACF, PACF)
plot(ACF_PACF)


# Al realizar las combinaciones entre ordenes tenemos los siguientes modelos:
# ARIMA  p,d,q
# ARIMA (2,1,1)
# ARIMA (4,1,1)


# Proceso de estimación ----

# d= orden de integración 
# p= orden de la parte autorregresiva AR(p)
# q= orden de la parte de medias móviles MA(q)

mod1 <- stats::arima(ts_newdeaths, order = c(2,1,1),
                     method = 'ML')
mod2 <- stats::arima(ts_newdeaths, order = c(4,1,1),
                     method = 'ML')
mod3 <- stats::arima(ts_newdeaths,order = c(2,1,0))



# Bondad de ajuste o selección el modelo optimo 

## Sustraer el AIC (Criterio de información Akaike)

AIC <- c(mod1$aic, mod2$aic, mod3$aic)
## Calcular el término de error cuadrático medio RMSE 

RMSE1 <- rmse(ts_newdeaths, fitted.values(mod1))
RMSE2 <- rmse(ts_newdeaths, fitted.values(mod2))
RMSE3 <- rmse(ts_newdeaths, fitted.values(mod3))


RMSE <- c(RMSE1, RMSE2,RMSE3)

## Construir un vector de nombres de los modelos 

Modelos <- c("Modelo 1: ARIMA (2,1,1)",
             "Modelo 2: ARIMA (4,1,1)",
             "Modelo A: ARIMA (2,1,0)")

## Constuir tabla de bondad de ajuste

Tabla_modelos <- data.frame(Modelos, AIC, RMSE)
print(Tabla_modelos)

##############################
# Nota: Se selecciono el modelo 4, ya que cuenta con el menor AIC y RMSE

# Evaluación del modelo -----

library(lmtest)

## Revisión general 

best_model <- mod2

summary(best_model)

#################### Evaluacion del modelo ##############

## Significancia individual 

coeftest(best_model)

## Raices o soluciones del modelo 

autoplot(best_model)+
  labs(title = 'Gráfico del circula unitario',
       subtitle = 'Soluciones del modelo ARIMA (4,1,1)')

#Dado que la raices de los polinomios se encuentran dentro
#del circulo unitario del plano Z,por lo que se puede asumir
#que el sistema se comporta de manera estable

## Valores reales vs estimados (en diferencia) 

fit <- fitted.values(best_model)

G1 <- ggplot(data=ts_newdeaths,
             aes(x=time(ts_newdeaths),
                 y=ts_newdeaths, colour='Fallecimientos reales'))+
  geom_line(size=0.8)+
  geom_line(data=fit,
            aes(y=fit, colour='Estimación ARIMA(4,1,1)'),
            size=0.8)+
  scale_x_continuous(breaks = seq(2005,2022,1))+
  scale_colour_manual(values=c('steelblue','red'))+
  labs(title = 'Fallecimientos semanales por COVID-19 reales vs estimados',
       subtitle = 'en México del 2020 al 2022',
       x='Fecha',
       y='Falleciminetos semanales',
       colour='Series')
plot(G1)

## Pruebas estadísticas del modelo 

### Pruebas de auto-correlación 

res_best_model <- residuals(best_model)

log10(124) # Rezagos optimos para realizar pruebas de autocorrelación

# Test Ljung-Box
Box.test(res_best_model, lag=2, type='Ljung-Box')

#Dado que el p-valor del Test Ljung-Box es 0.9734 y constrastado con un valor
#de significia del 0.05, no existe evidencia suficiente para rechazar H0, 
#es decir que no existe correlación serial en los residuos

## Prueba de Box-Pierce 

Box.test(res_best_model, lag=2, type= 'Box-Pierce')

#Dado que el p-valor del Test Box-Pierce es 0.9742 y constrastado con un valor
#de significia del 0.05,no existe evidencia suficiente para rechazar H0, 
#es decir que no existe correlación serial en los residuos


## Función de autocorrelación simple 

Acf(res_best_model, lag.max=24, plot=F) %>% 
  autoplot()+
  scale_x_continuous(breaks = seq(1,24,1))+
  labs(title = 'ACF de los residuales',
       x=expression(k),
       y=expression(rho))

# A pesar de que el grafico ACF muestras los resagos 12 y 13 fuera
# del intervalo de confianza, los estadisticos Box-Pierce y Ljung-Box, indican
# ausencia de autocorrelación en los rezagos, de tal modo que se
# asume que los residulos de modelo no presentan autocorrelacion serial


## Heterocedasticidad 

### Prueba ARCH 

library(aTSA)

arch.test(best_model)

### Residuales al cuadrado con ACF 

Acf(res_best_model^2, lag.max=24, plot=F) %>% 
  autoplot()+
  scale_x_continuous(breaks = seq(1,24,3))+
  labs(title = 'ACF de los residuales al cuadrado',
       x=expression(k),
       y=expression(rho))

Box.test(res_best_model^2, lag=24, type='Ljung-Box')

## Normalidad de residuales

jarque.bera.test(res_best_model)
shapiro.test(res_best_model)

## Analizar la estacionariedad 

tseries::adf.test(res_best_model)
tseries::pp.test(res_best_model)
tseries::kpss.test(res_best_model)

#Los residuos se comporta de manera estacionaria, ay que los estadisitcos
#ADF y PP rechazan la H0 de no estacionariedad, mientras que el test KPSS
#no hay suficiente evidencia para rechazar H0 de estacionariedad.
#De modo que aunque no existe normalidad en los residuos, si existe 
#estacionariedad en los datos, es decir, se cumple el supuesto que
#media = cte, varianza= cte y cov proxima a cero

# Pronostico ----

fcast <- forecast::forecast(best_model, h=5, level=95)
print(fcast)

autoplot(fcast)

Estimado_arima <- c(1:6)
Estimado_arima[1] <- ts_newdeaths[126]
Estimado_arima[2:6] <- (fcast$mean)
Estimado_arima=Estimado_arima <-ts(data=Estimado_arima,
                     start = c(2022,22), end=c(2022,27),
                     frequency = 52)

Estimado_up<- (fcast$upper)
Estimado_down<- (fcast$lower)
Estimado_up=Estimado_up <-ts(data=Estimado_up,
                                   start = c(2022,23), end=c(2022,27),
                                   frequency = 52)
Estimado_down=Estimado_down <-ts(data=Estimado_down,
                             start = c(2022,23), end=c(2022,27),
                             frequency = 52)


ggplot()+
  geom_line(data=ts_newdeaths, aes(x=time(ts_newdeaths), y=ts_newdeaths, 
                                   colour='Fallecimientos historicos'),size=0.8)+
  geom_line(data=Estimado_arima,aes(x=time(Estimado_arima),y=Estimado_arima,
                                     colour='Pronostico ARIMA(4,1,1)'),size=0.8)+
  geom_ribbon(aes(x=time(Estimado_down), ymin=Estimado_down, 
                  ymax=Estimado_up,fill = "Intervalo de confianza \nal 95%"), alpha = 0.3)+
  scale_y_continuous(breaks = seq(min(covid_week$new_deaths),
                                  max(covid_week$new_deaths),(100)))+
  scale_colour_manual(values=c('steelblue','red'))+
  scale_fill_manual("",values="grey12")+
  labs(title = "Forecasting fallecimientos semanales por COVID-19 en México",
       x='Fecha',
       y='Fallecimientos',
       colour='Series')
