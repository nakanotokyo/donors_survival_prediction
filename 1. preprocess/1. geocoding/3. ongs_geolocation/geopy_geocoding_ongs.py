
###############################################################################

import pandas as pd
from geopy.geocoders import Nominatim
from geopy.exc import GeocoderTimedOut
import codecs
import logging

#------------------ CONFIGURATION -------------------------------
# Set your output file name here.
output_filename = './ongs_domicilio_geocodificados.csv'
# Set your input file here
input_filename = "./ong_domicilio.csv"
# Specify the column name in your input data that contains addresses here
address_column_name = "DOMICILIO_ONGS" 
id_column_name = "ORGANIZATION_ID" 

logging.basicConfig(filename='./ongs_domicilio_geocodificados_log.log', level=logging.INFO, Format = '%(asctime)s:%(levelname)s:%(message)s')

#------------------ DATA LOADING --------------------------------


data = pd.read_csv(input_filename, sep = ';')  # si tira error : en sublime save with encodings utf-8

if address_column_name not in data.columns:
	raise ValueError("Missing Address column in input data")


addresses = data[address_column_name].tolist()
ids = data[id_column_name].tolist()

addresses = (data[address_column_name] +',Argentina').tolist()
ids = (data[address_column_name]).tolist()
#addresses = (data[address_column_name] +',Argentina').tolist()

#------------------ GEO CODING --------------------------------
user = 'xxxxx' # account name from Open Street Map

geolocator = Nominatim(user_agent= user, timeout = 5)

columns = ['id_row','ong_domicilio','ong_domicilio_osm','ong_lat','ong_lon']
lst = []

# i = 1 , elemento = 0 para empezar el proceso, los cambias si se corto y necesitas retomar desde ese punto
i = 1 # indice para la tabla
elemento = 0 # numero de item en donde quedo la iteracion de la lista addresses
f= codecs.open(output_filename,"a+", "utf-8-sig")
        
def do_geocode(address):
    try:
        return geolocator.geocode(address)
    except GeocoderTimedOut:
        return do_geocode(address)
        logging.info('do_geocode time out')
    
for address in addresses[elemento:]:
    location = do_geocode(address)
    if location == 'None' or location == None:
        f.write("%s;%s;%s;%s \n" % (i,None,None,None))
    else:            
        f.write("%s;%s;%s;%s \n" % (i,address,location.latitude,location.longitude))
    i = i+1
    
f.close()  




        
    
    
    