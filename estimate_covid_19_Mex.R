## Tarea 3: modelos lineales de predicción MCO

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


##install.packages("forecast",dependency=TRUE)

#Descargando la data sobre covid mundial
data <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")

#Filtrando los datos de Mexico
data <- data[data$location=="Mexico",]
#view(data)

#Filtrando datos de interes:
covid_mx <- select(data, date, new_deaths)
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
                                 max(covid_week$new_deaths),(1000)))+
  geom_area(alpha=0.6)+
  theme_minimal()+
  labs(title="Nuevos fallecimentos semanales por COVID-19",
       subtitle = "del 2020 al 2022 en Mexico",
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

li### Estadisticos por mes
covid_mensual<-covid_week %>% 
  group_by(mes_tex) %>% 
  summarise(min_anual=min(new_deaths),
            max_anual=max(new_deaths),
            prom_anual=mean(new_deaths),
            std_anual=sd(new_deaths))

covid_mensual


#box plot
covid_week %>% 
  filter(anio==2020) %>% 
  ggplot(aes(x=mes_tex, y=new_deaths))+
  geom_boxplot(fill="steelblue", color="black",
               outlier.colour = "red")+
  theme_minimal()+
  labs(title = "Gráfico de caja y bigotes fallecimientos semanales por covid en México para el año 2020",
       caption = "Elaboración propia con con datos de Our World in Data COVID-19",
       x="Fecha",
       y="Número de fallecimientos")

covid_week %>% 
  filter(anio==2021) %>% 
  ggplot(aes(x=mes_tex, y=new_deaths))+
  geom_boxplot(fill="steelblue", color="black",
               outlier.colour = "red")+
  theme_minimal()+
  labs(title = "Gráfico de caja y bigotes fallecimientos semanales por covid en México para el año 2021",
       caption = "Elaboración propia con con datos de Our World in Data COVID-19",
       x="Fecha",
       y="Número de fallecimientos")

covid_week %>% 
  filter(anio==2022) %>% 
  ggplot(aes(x=mes_tex, y=new_deaths))+
  geom_boxplot(fill="steelblue", color="black",
               outlier.colour = "red")+
  theme_minimal()+
  labs(title = "Gráfico de caja y bigotes fallecimientos semanales por covid en México para el año 2022",
       caption = "Elaboración propia con con datos de Our World in Data COVID-19",
       x="Fecha",
       y="Número de fallecimientos")


covid_week %>% 
  group_by(anio) %>% 
  ggplot(aes(x=anio, y=new_deaths))+
  geom_boxplot(fill="steelblue", color="black",
               outlier.colour = "red")+
  theme_minimal()+
  labs(title = "Gráfico de caja y bigotes fallecimientos semanales por covid en México para el año 2022",
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
  labs(title = "Histograma muertes semanales por Covid-19 en México",
       x="Clases",
       y="Frecuencia")

## Q-Q plot
ggplot(covid_week, aes(sample=new_deaths))+
  stat_qq(color="#2E86C1")+
  stat_qq_line(color="#C0392B", size=0.7)+
  theme_minimal()+
  labs(title = "Gráfico Q-Q fallecimientos semanales por Covid-19 en México")



## Conviertiendo los datos en series de tiempo  
ts_newdeaths<-ts(data=covid_week$new_deaths,
                start = c(2020,1),
                frequency = 52)



## Aplicación de una prueba jarque-bera 

jarque.bera.test(covid_week$new_deaths) 

## NOTA: La serie de tiempo naturalmente
## no presentará una distribución normal

adf.test(covid_week$new_deaths)


#Descomposición temporal

des_new_deaths<-decompose(ts_newdeaths)

plot(des_new_deaths)

# Analsis de la estacionariedad serie de tiempo a niveles I(0):
#1. Funciones de autocorrelación simple.

Acf(ts_newdeaths, lag.max=50, plot = F) %>% 
  autoplot()+
  labs(title= 'Funcion de correlación simple',
       subtitle = 'Serie de tiempo fallecimientos semanales por Covid-19 en México a niveles I(0)',
       y=expression(rho),
       x='k')+
  scale_x_continuous(breaks = seq(0,50,1))
#1.1 Funciones de autocorrelación parcial

Pacf(ts_newdeaths, lag.max=156, plot = F) %>% 
  autoplot()+
  labs(title= 'Funcion de correlación parcial',
       subtitle = 'Serie de tiempo fallecimientos semanales por Covid-19 en México a niveles I(0)',
       y=expression(rho),
       x='k')+
  scale_x_continuous(breaks = seq(0,156,52))
  
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
  scale_y_continuous(breaks = seq(min(covid_week$new_deaths),
                                  max(covid_week$new_deaths),(1000)))+
  theme_minimal()+
  labs(title="Nuevos fallecimentos semanales por COVID-19 del 2020 al 2022 en Mexico",
       subtitle = "Serie de tiempo con primera diferencia I(1)",
       caption = "Elaboración propia con con datos de Our World in Data COVID-19",
       x="Fecha",
       y="Número de fallecimientos")


# Analsis de la estacionariedad serie de tiempo en primer diferencia I(1):
#1. Funciones de autocorrelación simple.

Acf(ts_dif_newdeaths, lag.max=50, plot = F) %>% 
  autoplot()+
  labs(title= 'Funcion de correlación simple',
       subtitle = 'Serie de tiempo fallecimientos semanales por Covid-19 en México a niveles I(0)',
       y=expression(rho),
       x='k')+
  scale_x_continuous(breaks = seq(0,50,1))

# Se sospecha de estacionariedad ya que existe un caida exponencial en los  
# primeros retardos. Sin embargo, es necesario aplicar las pruebas de estacio-
# nariedad KPSS, DF y PP 

#1.1 Funciones de autocorrelación parcial

Pacf(ts_dif_newdeaths, lag.max=156, plot = F) %>% 
  autoplot()+
  labs(title= 'Funcion de correlación parcial',
       subtitle = 'Serie de tiempo fallecimientos semanales por Covid-19 en México a niveles I(0)',
       y=expression(rho),
       x='k')+
  scale_x_continuous(breaks = seq(0,156,52))

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
             en México \nElaboración propia con datos de Our World in 
             Data COVID-19')
ggseasonplot(ts_dif_newdeaths, year.labels = TRUE,
             year.labels.left = TRUE,
             main='Analisis de estacionalidad nuevos fallecimientos COVID-19 
             en México \nElaboración propia con datos de Our World in 
             Data COVID-19')


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
#que el valor de phi es != 0. y por lo tanto no hay precencia de raiz no 
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

# q = 1,12

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

mod1 <- stats::arima(ts_dif_newdeaths, order = c(2,1,1),
                     method = 'ML')
mod2 <- stats::arima(ts_dif_newdeaths, order = c(4,1,1),
                     method = 'ML')


# Bondad de ajuste o selección el modelo optimo 

## Sustraer el AIC (Criterio de información Akaike)

AIC <- c(mod1$aic, mod2$aic)
## Calcular el término de error cuadrático medio RMSE 

RMSE1 <- rmse(ts_dif_newdeaths, fitted.values(mod1))
RMSE2 <- rmse(ts_dif_newdeaths, fitted.values(mod2))


RMSE <- c(RMSE1, RMSE2)

## Construir un vector de nombres de los modelos 

Modelos <- c("Modelo 1: ARIMA (2,1,1)",
             "Modelo 2: ARIMA (4,1,1)")

## Constuir tabla de bondad de ajuste

Tabla_modelos <- data.frame(Modelos, AIC, RMSE)
print(Tabla_modelos)

##############################
# Nota: Se selecciono el modelo 4, ya que cuenta con el menor AIC y RMSE

# Evaluación del modelo -----

library(lmtest)

## Revisión general 

summary(mod4)

## Significancia individual 

coeftest(mod4)

## Raices o soluciones del modelo 

autoplot(mod4)+
  labs(title = 'Gráfico del circula unitario',
       subtitle = 'Soluciones del modelo ARIMA (2,1,3)')

## Valores reales vs estimados 

fit <- fitted.values(mod4)

G1 <- ggplot(data=ts_dif_newdeaths,
             aes(x=time(ts_dif_newdeaths),
                 y=ts_dif_newdeaths, colour='Barriles'))+
  geom_line(size=1)+
  geom_line(data=fit,
            aes(y=fit, colour='Estimado'),
            size=1)+
  scale_x_continuous(breaks = seq(2005,2022,1))+
  scale_colour_manual(values=c('steelblue','red'))+
  labs(title = 'Gráfico de valores reales vs estimados',
       subtitle = 'Modelo 4: ARIMA (2,1,3)',
       x='Fecha',
       y='Barriles',
       colour='Series')
plot(G1)

## Pruebas estadísticas del modelo 

### Correlación 

res_mod4 <- residuals(mod4)

## Prueba de Box-Pierce y Ljung-Box

log10(207) # Rezagos optimos para realizar pruebas de autocorrelación

Box.test(res_mod4, lag=2, type='Ljung-Box')

Box.test(res_mod4, lag=24, type='Ljung-Box')

qchisq(0.05, df=24, lower.tail = F)

Box.test(res_mod4, lag=2, type= 'Box-Pierce')

Box.test(res_mod4, lag=24, type= 'Box-Pierce')


# Nota: No hay correlación serial, ya que p-value es mayor que 0.05
# a un nivel de confianza del 5%, se obtiene el error tipo II. 

## Función de autocorrelación simple 

Acf(res_mod4, lag.max=24, plot=F) %>% 
  autoplot()+
  scale_x_continuous(breaks = seq(1,24,1))+
  labs(title = 'ACF de los residuales',
       x=expression(k),
       y=expression(rho))

## Prueba alternativa que junta Box-Pierce, histograma y ACF 

checkresiduals(mod4)

## Heterocedasticidad 

### Prueba ARCH 

library(aTSA)

arch.test(mod4)

### Residuales al cuadrado con ACF 

Acf(res_mod4^2, lag.max=24, plot=F) %>% 
  autoplot()+
  scale_x_continuous(breaks = seq(1,24,1))+
  labs(title = 'ACF de los residuales al cuadrado',
       x=expression(k),
       y=expression(rho))

Box.test(res_mod4^2, lag=24, type='Ljung-Box')

## Normalidad de residuales

jarque.bera.test(res_mod4)
shapiro.test(res_mod4)

## Analizar la estacionariedad 

tseries::adf.test(res_mod4)
tseries::pp.test(res_mod4)
tseries::kpss.test(res_mod4)

# Pronostico ----

fcast <- forecast::forecast(mod4, h=5, level=95)
print(fcast)

autoplot(fcast, include = 25)




