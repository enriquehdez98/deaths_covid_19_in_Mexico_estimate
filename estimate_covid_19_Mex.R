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



############## Seasonal ##########

ggseasonplot(ts_newdeaths, year.labels = TRUE,
             year.labels.left = TRUE,
             main='Analisis de estacionalidad nuevos fallecimientos COVID-19 en México \nElaboración propia con datos de Our World in Data COVID-19')
              




