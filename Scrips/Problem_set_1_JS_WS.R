#Autores: Juan Sebastián Vásquez Acevedo y Walter Leonardo Sánchez
#Curso: Big Data and Machine Learning
#PROBLEM SET 1
#Creación: 11/06/2022



## Instalar packages

##llamar packages
require(pacman)
p_load(tidyverse, rvest)


## Scraping de los datos en chunk
url_geih18<-"https://ignaciomsarmiento.github.io/GEIH2018_sample/"
browseURL(url_geih18)

## leer el html de la página del profe
html_t1 = read_html(url_geih18) 
class(html_t1)

## topar el Xpath para encontar las url de los chuns
html_t1 %>% html_nodes(xpath = 'html/body/div/div/div[2]/ul/li[1]/a')

## Extraer los elementos del nodo a correspndientes a los links
elements = html_t1 %>% html_elements("a")
elements

#Almacenar los links en un objeto
refs = elements %>% html_attr("href")
refs = refs[6:15]

titles = elements %>% html_text()
titles = titles[6:15]
dc1<-tibble(titles,url=paste0(url_geih18,refs))
titles

dc1 %>% head()

browseURL(dc1$url[1])

## Cargar datos



## Creación y modificación de variables



## Tablas y summarice



## Gráficos




## Regresiones y resultados



