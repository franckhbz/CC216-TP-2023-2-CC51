
setwd("C:/Users/franc/Escritorio/Materiales 2023-2/Fundamentos de Data Science/CC216-TP-2023-2-CC51/data/")

hotel_data<-read.csv("hotel_bookings.csv",header = TRUE, sep = ",",stringsAsFactors = FALSE)

hotel_data$agent<-factor(hotel_data$agent)
hotel_data$arrival_date_month<-factor(hotel_data$arrival_date_month,levels = month.name) ##month.name los ordena respecto al orden de meses
hotel_data$assigned_room_type<-factor(hotel_data$assigned_room_type)
hotel_data$company<-factor(hotel_data$company)
hotel_data$country<-factor(hotel_data$country)
hotel_data$customer_type<-factor(hotel_data$customer_type)
hotel_data$deposit_type<-factor(hotel_data$deposit_type)
hotel_data$distribution_channel<-factor(hotel_data$distribution_channel)
hotel_data$is_canceled<-factor(hotel_data$is_canceled)
hotel_data$is_repeated_guest<-factor(hotel_data$is_repeated_guest)
hotel_data$market_segment<-factor(hotel_data$market_segment)
hotel_data$meal<-factor(hotel_data$meal)
hotel_data$reservation_status<-factor(hotel_data$reservation_status)
hotel_data$reservation_status_date<-as.Date(hotel_data$reservation_status_date,format = "%Y-%m-%d")
hotel_data$reserved_room_type<-factor(hotel_data$reserved_room_type)
hotel_data$hotel<-factor(hotel_data$hotel)
sin_valor <- function(x){
  sum = 0
  for(i in 1:ncol(x))
  {
    cat("En la columna",colnames(x[i]),"total de valores NA:",colSums(is.na(x[i])),"\n")
  }
}
sin_valor(hotel_data)

en_blanco <- function(x){
  sum = 0
  for(i in 1:ncol(x))
  {
    cat("En la columna",colnames(x[i]),"total de valores en blanco:",colSums(x[i]==""),"\n")
  }
}
en_blanco(hotel_data)

## como nos hemos dado cuenta que hay 4 na en children pues los cambiaremos a 0, pues se entiende con na que hubo 0 niños.
hotel_data$children[is.na(hotel_data$children)] <- 0L


##para ver datos atipicos usaremos el diagrama de cajas a los tipos de datos numericos o enteros
library(DescTools)
library(mlr)  #summary

boxplot(hotel_data$lead_time, col = "deepskyblue",main="Boxplot Ventas",horizontal = TRUE)
boxplot(hotel_data$adr, col = "deepskyblue",main="Boxplot Ventas",horizontal = TRUE)
boxplot(hotel_data$children, col = "deepskyblue",main="Boxplot Ventas",horizontal = TRUE)

hotel_data <- subset(hotel_data, adr != 5400)#eliminamos el valor maximo de adr


# Pregunta a
library(ggplot2)

ggplot(hotel_data, aes(hotel,fill=hotel)) + geom_bar()+ylab("Cantidad de reservas") +
  theme_get()+ labs(title = "Reservas por tipo de hotel")+
  theme(plot.title = element_text(hjust = 0.5))

summary(hotel_data$hotel)

#Pregunta b
fechas <- as.Date(paste(hotel_data$arrival_date_year, as.numeric(hotel_data$arrival_date_month), hotel_data$arrival_date_day_of_month, sep = "-"))
#grafico 1 histograma
hist(fechas,breaks = "months", main = "Histograma de Fechas de llegada reservadas",
     xlab = "Fechas de llegada reservadas", ylab = "frecuencia",
     col = "steelblue")
Desc(fechas)
# Gráfico de líneas para mostrar la tendencia de la demanda a lo largo del tiempo
barplot(table(hotel_data$arrival_date_year), main="Tendencia de la Demanda con el Tiempo",col = "green")

#pregunta c y d
barplot(table(as.numeric(hotel_data$arrival_date_month)), main="Cantidad de reservas por mes",col = "green")

#pregunta e
c_reservas_con_menores<-sum(hotel_data$babies>0|hotel_data$children>0) 
print(paste("Existen ",c_reservas_con_menores,"reservas con bebes o niños"))

#pregunta f
c_necesitanestacionamiento<-sum(hotel_data$required_car_parking_spaces>0)
print(paste("La cantidad de reservas que si necesitan estacionamiento es:",c_necesitanestacionamiento))
c_nonecesitanestacionamiento<-sum(hotel_data$required_car_parking_spaces==0)
print(paste("La cantidad de reservas que no necesitan estacionamiento es:",c_nonecesitanestacionamiento))
barplot(table(hotel_data$required_car_parking_spaces), main="Cantidad de estacionamientos por reserva",col = "green")

#pregunta g

hotel_data_cancelados<-hotel_data[hotel_data$reservation_status=="Canceled",]
mes_estatus<-as.integer(format(hotel_data_cancelados$reservation_status_date, "%m"))   
barplot(table(mes_estatus), col="green", main = "Cantidad de reservas canceladas por mes")   
print(paste("La mayor cantidad de reservas canceladas es en el mes de Enero con",sum(mes_estatus==1)))


write.csv(hotel_data, file = "hotel_bookingsLimpio.csv", row.names = FALSE)
