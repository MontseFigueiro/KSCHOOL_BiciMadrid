#Download the file bicimadrid.csv
if(!file.exists("D:/master/data/Montse")){dir.create("D:/master/data/Montse")}
fileURL <- "http://datos.madrid.es/egob/catalogo/213155-3-bicimad-usos-usuarios.csv"
download.file(fileURL,destfile = "bicimadrid.csv")
datosbicimadrid <- read.csv("Bicimadrid2.csv",sep=";",header=TRUE)

#cheking the data
tail(datosbicimadrid)
head(datosbicimadrid,16)
dim(datosbicimadrid)
names(datosbicimadrid)
class(datosbicimadrid$Usos.bicis.total)
summary(datosbicimadrid)
##

#datosbicimadrid$new <- lapply(datosbicimadrid$Usos.bicis.total, function(x) as.numeric(gsub("\\.", "", as.character(x))))
#datosbicimadrid$Usos.bicis.total <-    gsub(".", "", datosbicimadrid$Usos.bicis.total, fixed = TRUE)
#datosbicimadrid$pruebatotal <-    gsub(datosbicimadrid$Usos.bicis.total,pattern='[.]',replacement='')
#datosbicimadrid$Usos.bicis.total <-    as.numeric(datosbicimadrid$Usos.bicis.total)
#sum(datosbicimadrid$Usos.bicis.total,na.rm=TRUE)----ERROR en numeros acabados en 0


#NA to 0
datosbicimadrid[is.na(datosbicimadrid$Usos.bicis.total),] <- 0


#Multiplicamos por 1000
datosbicimadrid$por1000 <- datosbicimadrid$Usos.bicis.total*1000

#Select col DIA and usos.bicis.totales

datosutiles <- datosbicimadrid[, names(datosbicimadrid) %in% c("DIA", "por1000")] 

#Change rows 1:15
datosutiles[1:15,"por1000"] <- (datosutiles[1:15,"por1000"])/1000
sum(datosutiles$por1000)
dim(datosutiles)

#as.Date of col DIA
class(datosutiles$DIA) #Factor
datosutiles$DIA <- as.Date(datosutiles$DIA,"%d/%m/%Y")
head(datosutiles)
usostotales <- datosutiles[order(datosutiles$DIA),]
head(usostotales)
sum(usostotales$por1000) #4691538

##EXTRACT MONTH
#install.packages("lubridate")
library(lubridate)
#usostotales$month <- month(usostotales$DIA)
#usostotales$year <- year(usostotales$DIA)


##New variable with year and month
#install.packages("zoo")
library(zoo)
usostotales$yearmon <- as.yearmon(paste(year(usostotales$DIA), month(usostotales$DIA), sep = "-"))
usostotales$yearmon <- as.Date(paste('01', usostotales$yearmon), format='%d %b %Y')
head(usostotales) # look at it

#first15 <- datosutiles[1:15,names(datosutiles) %in% c("DIA","por1000")]
#first15$por1000 <- (first15$por1000)/1000 
#list <- data.frame(datosbicimadrid[1:15,"por1000"]/1000)


#Extract a data frame for every year

usostotales2014 <- usostotales[year(usostotales$DIA)==2014,]
totalesmes2014 <- aggregate(usostotales2014$por1000 ~ month(usostotales2014$DIA),FUN=sum,na.rm=TRUE)
names(totalesmes2014) <- c("date","Totalusobicis")
sum(totalesmes2014$Totalusobicis)
#726662

usostotales2015 <- usostotales[year(usostotales$DIA)==2015,]
totalesmes2015 <- aggregate(usostotales2015$por1000 ~ month(usostotales2015$DIA),FUN=sum,na.rm=TRUE)
names(totalesmes2015) <- c("date","Totalusobicis")
sum(totalesmes2015$Totalusobicis)
#3075454


usostotales2016 <- usostotales[year(usostotales$DIA)==2016,]
totalesmes2016 <- aggregate(usostotales2016$por1000 ~ month(usostotales2016$DIA),FUN=sum,na.rm=TRUE)
names(totalesmes2016) <- c("date","Totalusobicis")
sum(totalesmes2016$Totalusobicis)
#889422



#Plot for every year
library(ggplot2)
ggplot(data=totalesmes2014, aes(x=totalesmes2014$date,y=totalesmes2014$Totalusobicis)) +geom_line()+labs(x="Months",y="Usos Totales Bicis")


##Agregate by month and sum "Usos.bicis.total"
TotalBicisMonth <-  aggregate(usostotales$por1000 ~ usostotales$yearmon, FUN = sum, na.rm=TRUE)
names(TotalBicisMonth) <- c("date", "usosbicitotal")
sum(TotalBicisMonth$usosbicitotal,na.rm=TRUE)
class(TotalBicisMonth$date) 


#TotalBicisMonth$month <- month(TotalBicisMonth$date)
#TotalBicisMonth <- transform(TotalBicisMonth, MonthAbb = month.abb[TotalBicisMonth$date])
#TotalBicisMonth$MonthAbb <- NULL

TotalBicisMonth$namescol <- paste(month.name[month(TotalBicisMonth$date)],year(TotalBicisMonth$date))
row.names(TotalBicisMonth) <- TotalBicisMonth$namescol
xticks <- row.names(TotalBicisMonth)
class(xticks)


#Plot
png("Plot1.png")
plot(TotalBicisMonth$date,TotalBicisMonth$usosbicitotal,col="red",sub="Datos de junio 2014 a abril 2016",main="Evoluci�n Uso BiciMadrid",xlab="Meses",ylim=range(0:400000),ylab="N�mero de Usos mensuales")
dev.off()
years <- unique(year(TotalBicisMonth$date))
years

png("Plot2.png")
ggplot(data = TotalBicisMonth, aes(x = TotalBicisMonth$date, y = TotalBicisMonth$usosbicitotal, group = 1))+geom_line(colour = "blue") 
dev.off()

