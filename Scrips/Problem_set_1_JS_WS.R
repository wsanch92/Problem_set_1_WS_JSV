#Autores: Juan Sebastián Vásquez Acevedo y Walter Leonardo Sánchez
#Curso: Big Data and Machine Learning
#PROBLEM SET 1
#Creación: 11/06/2022



## Instalar packages
#install.packages("XML")

##llamar packages
require(pacman)
p_load(tidyverse, rvest, tibble,rio,skimr,caret)



## Scraping de los datos en chunk
url_geih18 <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/"
browseURL(url_geih18)

## leer el html de la página del problem set
html_t1 <- read_html(url_geih18) 
class(html_t1)

## tomar el Xpath para encontar las url de los chuns
html_t1 %>% html_nodes(xpath = 'html/body/div/div/div[2]/ul/li[1]/a')

## Extraer los elementos del nodo a correspndientes a los links
elements <- html_t1 %>% html_elements("a")
elements

#Almacenar los links que necesitamos en un objeto
refs <- elements %>% html_attr("href")
x_inicio<-(length(refs)-10)+1
refs <- refs[x_inicio:length(refs)]
refs

#Referenciar los nombres de los chunk y enlistar las url obtenidad para cada chunk en un tibble
nom_chunk <- elements %>% html_text()
nom_chunk <- nom_chunk[x_inicio:length(nom_chunk)]
db_url<-tibble(nom_chunk,url=paste0(url_geih18,refs))

## Revisar el tibble de urls
db_url %>% head()
db_url$url[1]

# Rango de filas que corresponde al número de chunks
n <- nrow(db_url)
n
#Se crea un Data Frame vacio para agregar todas las bases obtenidas 
db <- data.frame()

#Elaboración de loop para agregar todas las particiones de la GEIH
for (i in 1:n){
  urls <- db_url$url[i] %>% read_html() %>% html_elements("div") %>% html_attr("w3-include-html") #se obtine la parte de url verdadera
  urls <- urls[8] #Exploración del elemento 8 donde esta la url verdadera
  dc<-tibble(nom_chunk,urls=paste0(url_geih18,urls)) #unir la url principal con la parte de url verdadera
  dbc <- dc$urls[i] %>% read_html() %>% html_nodes("table") %>% html_table() %>% as.data.frame() # extracción de la tabla y conversión en DataFrame
  db <- rbind(db, dbc) # append de las bases
  cat("chunk OK:", i, ", ")
}

db <- as_tibble(db)

saveRDS(object = db, file = "C:/Users/walte/OneDrive/Documentos/Maestría en Economía Aplicada/Big Data/GitHub/Talleres/Problem_set_1_WS_JSV/Datos_geih_bogota.rds")


## Cargar datos



## Creación y modificación de variables

## Base con personas mayores de 18 años y que esten ocupados

db_1 <- db_1 %>% mutate(db_1,)
db_1 <- db_1[db_1$age >= 18 & db_1$ocu == 1, ]

## Tablas y summarice

## Resumen de toda la base

skim(db)

## Gráficos




## Regresiones y resultados



