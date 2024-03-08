library(httr)
library(jsonlite)

#Este script lee la API de los establecimientos de salud en Chile que esta alojada en el siguiente link
#https://datos.gob.cl/dataset/establecimientos-de-salud-vigentes/resource/2c44d782-3365-44e3-aefb-2c8b8363a1bc


# Definir la URL de la API
url <- "https://datos.gob.cl/api/3/action/datastore_search?resource_id=2c44d782-3365-44e3-aefb-2c8b8363a1bc&limit=17000"

# Realizar la solicitud GET a la API
response <- GET(url)
content <- content(response, "text")
data <- fromJSON(content)
establecimientos_nacional <- data$result$records

rm(data, response, content, url)
