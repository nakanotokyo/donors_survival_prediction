
############GEOCODING DE LOCALIZACIONEs: GENERACION DE DATASET PARA PYTHON#####################

#ONG
DROP TABLE IF EXISTS Z_GEO_ONG_DOMICILIO;
CREATE TABLE Z_GEO_ONG_DOMICILIO AS
SELECT
	DISTINCT(ORGANIZATION_ID),
	ONG_NAME,
	ADDRESS_1,
	ADDRESS_2,
	LOWER(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(DOMICILIO_ONG,'á','a'),'é','e'),'í','i'),'ó','o'),'ú','u')) DOMICILIO_ONGS
FROM
	(SELECT 
		ID AS ORGANIZATION_ID,
		NAME AS ONG_NAME,
		ADDRESS_1,
		ADDRESS_2,
		CASE WHEN ADDRESS_1 IS NULL OR ADDRESS_1 = '' OR TRIM(ADDRESS_1) = '' THEN NULL 
		 	 WHEN ADDRESS_2 IS NULL THEN CONCAT(ADDRESS_1,',',CITY,',',STATE)
		 	 WHEN ADDRESS_2 IS NOT NULL THEN CONCAT(ADDRESS_1,' ',ADDRESS_2,',',CITY,',',STATE) 
		 	 ELSE NULL END AS DOMICILIO_ONG
	FROM 
		ORGANIZATIONS) A
-- WHERE DOMICILIO_ONG IS NOT NULL;


#DONORS
DROP TABLE IF EXISTS Z_GEO_DONOR_DOMICILIO;
CREATE TABLE Z_GEO_DONOR_DOMICILIO AS
(SELECT 
	* 
FROM
(SELECT
	DONOR_ID,
	CASE WHEN DONOR_DOMICILIO IS NULL OR DONOR_DOMICILIO = '' THEN NULL ELSE CONCAT(DONOR_DOMICILIO,',',LOCALIDAD)END AS DOMICILIO_DONOR
FROM 
	(SELECT
		DONOR_ID,
		DONOR_DOMICILIO,
		CASE WHEN DONOR_CITY IS NULL OR DONOR_CITY = '' THEN DONOR_STATE ELSE DONOR_CITY END AS LOCALIDAD
	FROM 
		Z_BASE_DONACIONES) AA
	WHERE 
		LOCALIDAD != '-' 
		AND LOCALIDAD NOT LIKE '%XX%' 
		AND LOCALIDAD IS NOT NULL) BB
WHERE 
	DOMICILIO_DONOR IS NOT NULL 
	AND DOMICILIO_DONOR != '--,--' 
	AND DOMICILIO_DONOR != '*,*');


# MATCH: DONOR ID Y DONOR DOMICILIO
DROP TABLE IF EXISTS Z_GEO_DONOR_ID_ONG_ID;
CREATE TABLE Z_GEO_DONOR_ID_ONG_ID (PRIMARY KEY(DONOR_ID)) AS
SELECT
	DISTINCT DONOR_ID,
	ORGANIZATION_ID
FROM
	DONORS A
LEFT JOIN
	DONATIONS B
ON A.ID = B.DONOR_ID
LEFT JOIN
	CAMPAIGNS C
ON B.CAMPAIGN_ID = C.ID
LEFT JOIN
	ORGANIZATIONS D
ON C.ORGANIZATION_ID = D.ID
WHERE D.COUNTRY = 'AR'
AND D.ENABLED_AT IS NOT NULL;


# IMPORT DE DATASET RESULTADO DE PYTHON
# Tabla con distncias
-- DROP TABLE IF EXISTS Z_GEO_DISTANCIAS;
CREATE TABLE Z_GEO_DISTANCIAS
(
	ORGANIZATION_ID BIGINT,
	DONOR_ID BIGINT,
	DONOR_LAT FLOAT,
	DONOR_LON FLOAT,
	LAT_ONG FLOAT,
	LON_ONG FLOAT,
	DISTANCE BIGINT,
	PRIMARY KEY(DONOR_ID)
);


############# ANALISIS FALTANTES ##############

# ONG

SELECT COUNT(*) FROM ORGANIZATIONS;
SELECT SUM(CASE WHEN DOMICILIO_ONGS IS NULL THEN 1 ELSE 0 END) AS Q_NULL, COUNT(*) FROM Z_GEO_ONG_DOMICILIO;

SELECT * FROM Z_GEO_ONG_DOMICILIO WHERE DOMICILIO_ONGS IS NULL;


# ONGS QUE HAY Q COMPLETAR DIRECCION PORQUE TIENEN MUCHOS DONANTES
SELECT
	D.ID,	
	D.NAME,
	COUNT(DONOR_ID)	
FROM
	DONATIONS A
LEFT JOIN
	DONORS B
ON A.DONOR_ID = B.ID
LEFT JOIN
	CAMPAIGNS C
ON A.CAMPAIGN_ID = C.ID
LEFT JOIN
	ORGANIZATIONS D
ON C.ORGANIZATION_ID = D.ID
LEFT JOIN	
	Z_GEO_ONG_DOMICILIO E
ON D.ID = E.ORGANIZATION_ID
WHERE 
	E.DOMICILIO_ONGS IS NULL
GROUP BY 1,2;
