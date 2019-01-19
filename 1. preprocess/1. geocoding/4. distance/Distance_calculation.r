library(geosphere)
library(naniar)
library(tidyr)

options(scipen=999)
########################################################### ONGs #############################################################
# Geocodificado
file <- "~/ongs_domicilio_geocodificados.csv"
ong_domicilio <-read.csv(file = file, header = FALSE, sep = ';')
colnames(ong_domicilio) <- c('row','domicilio','lat','long') 

# Original
file <- "~/ong_domicilio.csv"
ong_domicilio_ids <-read.csv(file = file, header = TRUE, sep = ';')
colnames(ong_domicilio) <- c('row','domicilio','lat_ong','lon_ong') 

# Merge
ongs_geocoded <- merge(ong_domicilio_ids,ong_domicilio, by=0, all=TRUE)

ongs_geocoded$Row.names <- NULL
ongs_geocoded$row <- NULL
rm(ong_domicilio)
rm(ong_domicilio_ids)

# Export nulls
ongs_missing <- ongs_geocoded[ongs_geocoded[, "domicilio"] == 'None',]
write.csv(ongs_missing, file = "~//ongs_missing.csv")

# Elimino nulls de dataset
ongs_geocoded = ongs_geocoded %>% replace_with_na(replace = list(lat_ong = 'None'))
ongs_geocoded = ongs_geocoded %>% replace_with_na(replace = list(lon_ong = 'None '))
ongs_geocoded <- ongs_geocoded %>% drop_na(lat_ong)

# Importo nulls completados
file <- "~//ongs_missing_completed.csv"
ongs_missing_completed <-read.csv(file = file, header = TRUE, sep = ';')
ongs_geocoded$lat_ong <- as.numeric(as.character(ongs_geocoded$lat_ong))
ongs_geocoded$lon_ong <- as.numeric(as.character(ongs_geocoded$lon_ong))

# Union de las ong que ahora tienen datos completos
ongs_geocoded_complete <- rbind(ongs_geocoded,ongs_missing_completed)

rm(ongs_geocoded)
rm(ongs_missing_completed)

ongs_geocoded <- ongs_geocoded_complete
ongs_geocoded$domicilio <- NULL
rm(ongs_geocoded_complete)

########################################################### Donors #############################################################
# Geocodificado
file <- "~/donors_domicilio_geocodificados.csv"
donor_domicilio <-read.csv2(file = file, header = TRUE, sep = ';')
colnames(donor_domicilio) <- c("donor_id", "donor_domicilio", "donor_domicilio_osm","donor_lat","donor_lon") 

########################################################### Match #############################################################
# Importo MATCH DONORS Y ONGS IDS
file <- "~//donor_ids_vs_ong_ids.csv"
match <-read.csv2(file = file, header = TRUE, sep = ';')

########################################################### JOIN #############################################################
df <- merge(donor_domicilio,match, by.x = 'donor_id', by.y ='DONOR_ID' , all.x = TRUE)
df <- merge(df,ongs_geocoded, by.x = 'ORGANIZATION_ID', by.y = 'ORGANIZATION_ID',all.x = TRUE)

df = df %>% replace_with_na(replace = list(lat_ong = 'None'))
df = df %>% replace_with_na(replace = list(lon_ong = 'None '))
df = df %>% replace_with_na(replace = list(donor_lat = 'None'))
df = df %>% replace_with_na(replace = list(donor_lon = 'None '))

# Calculo de distancia ejemplo
lon1 = 58.3898976530612
lat1 = 34.6327392653061
lon2 =58.4375037795918
lat2= 34.5850681040816

# Me quedo solo con casos completos
data_complete <- df[complete.cases(df), ] 
rm(df)
rm(match)
rm(ongs_missing)
rm(ongs_geocoded)
rm(donor_domicilio)
# Convierto longitud y latitud a numericos (para eso primero tengo q pasarlos a character)
data_complete$donor_lon <- as.numeric(as.character(data_complete$donor_lon))
data_complete$donor_lat <- as.numeric(as.character(data_complete$donor_lat))
data_complete$lon_ong <- as.numeric(as.character(data_complete$lon_ong))
data_complete$lat_ong <- as.numeric(as.character(data_complete$lat_ong))

# Borro los registros erroneos: latitudes y longitudes que no podrian ser de Argentina. 
data_complete<-data_complete[!(data_complete$donor_lon < - 80),]
data_complete<-data_complete[!(data_complete$donor_lon> - 50),]
data_complete<-data_complete[!(data_complete$donor_lat > - 10),]
data_complete<-data_complete[!(data_complete$donor_lat < - 60),]

# Aplico formula para calcular distancia
distance <- distm(data_complete[,5:6], data_complete[,8:9])
dist <- distance[,c(1)]
distancia <- as.data.frame(round(dist))
data4export <- cbind(data_complete,distancia)
##########################################################OUTPUT FILES##############################################################
data4export$donor_domicilio <- NULL
data4export$donor_domicilio_osm <- NULL
data4export$DOMICILIO_ONGS <- NULL
colnames(data4export)[colnames(data4export)=="round(dist)"] <- "distance"

write.csv2(data4export, file = "~//results/geocoded.csv", row.names = FALSE, col.names = TRUE)

