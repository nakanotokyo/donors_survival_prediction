
###############################################################################

import pandas as pd
from geopy.geocoders import Nominatim
from geopy.exc import GeocoderTimedOut
import codecs
import logging

#------------------ CONFIGURATION -------------------------------
# Set your output file name here.
output_filename = './donors_domicilio_geocodificados.csv'
# Set your input file here
input_filename = "./donors_domicilios.csv"
# Specify the column name in your input data that contains addresses here
address_column_name = "donor_domicilio" 
id_column_name = 'donor_id'

logging.basicConfig(filename='./donors_domicilio_geocodificados_final_log.log', level=logging.INFO, Format = '%(asctime)s:%(levelname)s:%(message)s')

#------------------ DATA LOADING --------------------------------
data = pd.read_csv(input_filename, sep = ';')


if address_column_name not in data.columns:
	raise ValueError("Missing Address column in input data")
    
#------------------ DATA PREPROCESS --------------------------------    
# Remove punctuation and accents
data['donor_domicilio'] = data['donor_domicilio'].str.replace('.','')
data['donor_domicilio'] = data['donor_domicilio'].str.replace('á','a')
data['donor_domicilio'] = data['donor_domicilio'].str.replace('é','e')
data['donor_domicilio'] = data['donor_domicilio'].str.replace('í','i')
data['donor_domicilio'] = data['donor_domicilio'].str.replace('ó','o')
data['donor_domicilio'] = data['donor_domicilio'].str.replace('ú','u')

data["donor_ciudad"] = data["donor_ciudad"].str.replace('.','')
data["donor_ciudad"] = data["donor_ciudad"].str.replace('á','a')
data["donor_ciudad"] = data["donor_ciudad"].str.replace('é','e')
data["donor_ciudad"] = data["donor_ciudad"].str.replace('í','i')
data["donor_ciudad"] = data["donor_ciudad"].str.replace('ó','o')
data["donor_ciudad"] = data["donor_ciudad"].str.replace('ú','u')
data["donor_ciudad"] = data["donor_ciudad"].str.replace('CABA','Ciudad Autonoma de Buenos Aires')
data["donor_ciudad"] = data["donor_ciudad"].str.replace('caba','Ciudad Autonoma de Buenos Aires')
data["donor_ciudad"] = data["donor_ciudad"].str.replace('c.a.b.a.','Ciudad Autonoma de Buenos Aires')
data["donor_ciudad"] = data["donor_ciudad"].str.replace('Bs As','Buenos Aires')


addresses = data[address_column_name].tolist()

addresses = (data[address_column_name] + ',' + data['donor_ciudad'] +',Argentina').tolist()

#addresses = (data[address_column_name] +',Argentina').tolist()

identification = data[id_column_name].tolist()

#------------------ GEO CODING --------------------------------
# My account
user = 'xxx' # user name in Open Street Map 
geolocator = Nominatim(user_agent=user, timeout = 5)

# Geocoding function
def do_geocode(address):
    try:
        return geolocator.geocode(address)
    except GeocoderTimedOut:
        return do_geocode(address)
        logging.info('do_geocode time out')


# Output file format and headers
f= codecs.open(output_filename,"a+", "utf-8-sig") # para que el excel salga ok
header = 'donor_id','donor_domicilio','donor_domicilio_osm','donor_lat','donor_lon'
f.write("%s;%s;%s;%s;%s \n" % ('donor_id','donor_domicilio','donor_domicilio_osm','donor_lat','donor_lon'))
    
# Iteration and data saving
# Being and end are used for test purposes only. 
begin = 0
end = len(addresses)

for (donor_id, address) in zip(identification[begin:end],addresses[begin:end]):
    location = do_geocode(address)
    if location == 'None' or location == None:
        f.write("%s;%s;%s;%s;%s \n" % (donor_id,address,None,None,None))
    else:     
        f.write("%s;%s;%s;%s;%s \n" % (donor_id,address,location.address,location.latitude,location.longitude))
   
f.close()     


        
    
    
    