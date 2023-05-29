# Universidad de los Andes
# Taller de R: Estadística y Programación
# Fecha: 20/03/2023'
# Taller 1 
# Sergio Andres Melo Garcia - 202021527
rm(list = ls())
require(pacman) #Descargo librerias
p_load(tidyverse,rio,stargazer,coefplot) 

datos <- readRDS("C:/Users/smelo/Downloads/data_regresiones.rds") #importo datos

# 1. Regresiones

#1.1 Regresiones en 3 modelos
modelo_1 <- lm(price ~ rooms + dist_cbd, data = datos) #Regresión de precio sobre rooms y dist_cbd
modelo_2 <- lm(price ~ rooms + dist_cbd + bathrooms, data = datos) #regresión de precio sobre rooms, dist_cbd y bathrooms
modelo_3 <- lm(price ~ rooms + dist_cbd + bathrooms + surface_total, data = datos)  # Regresión de recio sobre rooms, dist_cbd, bathrooms y surface_total

#1.2 Grafica de la distribución de los coeficientes de los modelos
coefplot(model = modelo_3) + theme_test()

#1.3 
ggsave(filename = "output/plot_regresiones.png")
stargazer(modelo_1,modelo_2,modelo_3,
          type = "text", 
          out = "output/resultados_regresiones.xlsx") #exportación de la tabla y grafico de los coeficientes

#2
p_load(skimr,
       sf, 
       leaflet, 
       tmaptools, 
       ggsn,
       osmdata,
       ggplot2, 
       ggmap)

#2.1 
opq(bbox = getbb("Cali Colombia")) # descargo la caja que contiene el poligono de cali

rest <- opq(bbox = getbb("Cali Colombia")) %>% add_osm_feature(key="amenity" , value="restaurant") 
class(rest) #creo un objeto que contiene un feature de los restaurantes de cali, este objeto se conforma de los datos espaciales de los restaurantes en esta ciudad
parques <- opq(bbox = getbb("Cali Colombia")) %>% add_osm_feature(key="leisure" , value="park")   
class(parques) #realizo lo mismo para los parques de Cali, con la diferencua de que la forma espacial de este objeto sea un polígono

rest_1 = rest %>% osmdata_sf() # Se usa este ultimo comando para acceder a los features de los objetos previamente armados para restaurantes, de forma que este nuevo objeto incluya información en puntos y en polígonos. 

parques_1 <- parques %>% osmdata_sf() #Hago lo mismo para los parques 

#se crea objetos sf para terminar de descaragar la información de restaurantes y parques en Cali
restaurantescali =  rest_1$osm_points[,c("osm_id", "amenity")] #Con este comando se obtienen los puntos sobre los cuales hay restaurantes en cali
parquescali = parques_1$osm_polygons[,c("osm_id", "amenity")] #Con este comando se obtienen los poligonos en los cuales se encuentran los parques en cali


#2.2
leaflet() %>%  addCircles(data=restaurantescali, color="blue") #Con este comando se pueden ubicar los puntos donde hay restaurantes en cali, resaltados en color azul
leaflet() %>% addTiles() %>% addPolygons(data=parquescali) # Con este comando se trazan poligonos sobre los cuales hay parques en cali

#2.3 
clubcampestre_cali <- geocode_OSM("Calle 5 %20% 100, Cali", as.sf=T) #Con este comando geocodifico la dirección del Club Campestre de Cali, se utiliza "%20%" para que se reconozca la carrera. 

#3

# 3.1 Leer la URL y crear un objeto xml_document


URL <- "https://es.wikipedia.org/wiki"
PAG <- read_html(URL)

# 3.2 Extraer el título de la página utilizando XPath
titulo <- html_text(html_node(PAG, xpath = "//title"))

# 3.3 Extraer la tabla de los departamentos de Colombia y exportarla como archivo Excel
nodos_tabla <- html_nodes(PAG, xpath = "//table[contains(@class, 'wikitable')]") # Se utiliza html_nodes() con el XPath para seleccionar los nodos de la tabla que contiene los departamentos de Colombia en la página web. El XPath utilizado es "//table[contains(@class, 'wikitable')]". Esta expresión XPath selecciona los nodos <table> que contienen la clase CSS "wikitable".

tabla_departamento <- html_table(nodos_tabla, fill = TRUE) # se utiliza este comando para extraer la tabla de los nodos seleccionados. Se asigna el resultado a la variable tabla_departamento.

library(openxlsx) #se carga este paquete para trabajar sobre formato Excel

libro <- createWorkbook() # Se crea un nuevo libro de Excel utilizando la función createWorkbook()
addWorksheet(libro, "Departamentos") # Se agrega una hoja de cálculo llamada "Departamentos" al libro utilizando la función addWorksheet()
writeData(libro, sheet = "Departamentos", tabla_departamento) # se escribe la tabla de departamentos en la hoja de cálculo 

saveWorkbook(libro, file = "output/tabla_departamento.xlsx", overwrite = TRUE) #  se guarda el libro de Excel en un archivo

# 3.4 Extraer los párrafos y generar una nube de palabras
install.packages("tm")
install.packages("wordcloud")

library(tm)
library(wordcloud) 
#Se carga el paquete tm para trabajar con texto y el paquete wordcloud para generar una nube de palabras

parrafos <- html_text(html_nodes(PAG, xpath = "//p")) #Se utiliza  htlm_nodes para seleccionar los nodos de párrafos (<p>) en la página web.
# Tambien se usa html_text para extraer el texto de los nodos seleccionados y se guarda en la variable parrafos.

texto <- Corpus(VectorSource(parrafos)) #se crea este objeto el cual permita preparar los datos de texto el resto de procesamiento
texto <- tm_map(texto, content_transformer(tolower))
texto <- tm_map(texto, removePunctuation)
texto <- tm_map(texto, removeNumbers)
texto <- tm_map(texto, removeWords, stopwords("spanish"))
texto <- tm_map(texto, stripWhitespace)
# estos anteriores comandos convierte el texto a minúsculas, se eliminan puntuaciones, números y palabras de parada en español, y se eliminan los espacios en blanco adicionales.


nube_palabras <- wordcloud(texto, min.freq = 10, random.order = FALSE, colors = brewer.pal(8, "Dark2"))
#se genera la nube de palabras a partir del corpus de texto transformado. Se establece un mínimo de frecuencia de 10 y se utiliza el orden aleatorio para las palabras.


png("output/nube_palabras.png") # se guarda la nube de palabras generada en un archivo PNG 


#Este ultimo punto del codigo se realiza con ayuda de paginas web y chat GPT                               
                                 
                                                                  
                                 
                                 