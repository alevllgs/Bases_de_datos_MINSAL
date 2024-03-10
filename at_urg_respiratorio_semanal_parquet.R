# Carga el paquete arrow
# este script busca los datos web, los descarga y guarda en DF


library(arrow)

# Especifica la ruta del archivo Parquet
url_archivo  <- "https://datos.gob.cl/dataset/606ef5bb-11d1-475b-b69f-b980da5757f4/resource/ae6c9887-106d-4e98-8875-40bf2b836041/download/at_urg_respiratorio_semanal.parquet"

# Lee el archivo Parquet desde la URL
datos_parquet <- read_parquet(url_archivo)


# Visualiza los primeros registros del dataframe
head(datos_parquet)


