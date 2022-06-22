#Autores: Juan Sebastián Vásquez Acevedo y Walter Leonardo Sánchez
#Curso: Big Data and Machine Learning
#PROBLEM SET 1
#Creación: 11/06/2022



## Instalar packages
#install.packages("XML")

##llamar packages
require(pacman)
p_load(tidyverse, rvest, tibble,rio,skimr,caret,stargazer)



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


## Guardar base de datos de GEIH
saveRDS(object = db, file = "C:/Users/walte/OneDrive/Documentos/Maestría en Economía Aplicada/Big Data/GitHub/Talleres/Problem_set_1_WS_JSV/Datos_geih_bogota.rds")


## Cargar datos

db <- read_rds("/Users/usuario/Desktop/Problem_set_1_WS_JSV/Datos_geih_bogota.rds")


## Creación y tratamiento de variables
## ocupados  mayores de 18
db_1 <- db[db$age >= 18 & db$ocu == 1, ]

## Creación de lista con las variables elegibles
variables <- c('age','ingtot','ingtotes','ingtotob','p6210','p6210s1','p6240','relab','sex','fex_c','mes','p6426')
db_final <- db_1[,variables]  ## Se filtra la base por la lista de variables elegibles
db_final%>%head()
colnames(db_final)

## Renombrar variables
names (db_final) <- c('edad','ingreso','ingtotes','ingtotob','niveleduc','p6210s1','actividad','ocupacion','sex','fex_c','mes','tiempoempresa')

## Creación de lista de variables elegidas
vars_final <- c('edad','ingreso','niveleduc','actividad','ocupacion','sex','tiempoempresa')

db_final <- db_final %>% mutate(rangos_edad= case_when(edad <= 24 ~ "18-24", 
                                                       edad >= 25 & edad < 30 ~ "25-29", 
                                                       edad >= 30 & edad < 35 ~ "30-34", 
                                                       edad >= 35 & edad < 40 ~ "35-39",
                                                       edad >= 40 & edad < 45 ~ "40-44", 
                                                       edad >= 45 & edad < 50 ~ "45-49", 
                                                       edad >= 50 & edad < 55 ~ "50-54", 
                                                       edad >= 55 & edad < 60 ~ "55-59",
                                                       edad >= 60 & edad < 65 ~ "60-64", 
                                                       edad >= 65 & edad < 70 ~ "65-69", 
                                                       edad >= 70 & edad < 75 ~ "70-74", 
                                                       edad >= 75 & edad < 80 ~ "75-79",
                                                       edad >= 80 ~ "80 y más"))
## variables de población  para hacer la pirámide poblacional
db_final  <- db_final %>% mutate( pob_sex=1)


db_final <- db_final %>% mutate(db_final, edad_sqr=edad^2) ## edad al cuadrado
db_final <- db_final %>% mutate(db_final, tiempoempresa_sqr=tiempoempresa^2) ## experiencia al cuadrado medido en meses

############################ Tablas y summarice  ############################


## Resumen de la base con las variables elegidas

skim(db_final[,vars_final])

## Gráficos

ggplot() + geom_histogram(data=db_1 , aes(x=ingtot) , fill="coral1" , alpha=0.5) + 
  geom_histogram(data=db_1 , aes(x=ingtotes) , fill="blue" , alpha=0.5) +
  geom_histogram(data=db_1 , aes(x=ingtotob) , fill="red" , alpha=0.5)
#

ggplot() + geom_histogram(data=db_1 , aes(x=ingtot) , fill="coral1" , alpha=0.5) + 
  geom_histogram(data=db_1 , aes(x=ingtotob) , fill="blue1" , alpha=0.5)

ggplot(data = db_final , 
       mapping = aes(x = edad , y = ingreso , group=as.factor(sex) , color=as.factor(sex))) +
  geom_point()



ggplot(data = db_final , 
       mapping = aes(x = rangos_edad , y = edad , group=as.factor(sex) , color=as.factor(sex))) + 
         geom_col()


## Pirámide poblacional en muestra
ggplot(db_final, aes(x = `rangos_edad`, y =  pob_sex, fill = sex)) + 
  geom_col(data = subset(db_final, sex == 1) %>% mutate(pob_sex = -pob_sex), width = 0.5, fill = "blue") + 
  geom_col(data = subset(db_final, sex == 0) %>% mutate(pob_sex = pob_sex), width = 0.5, fill = "pink") + coord_flip() +
  scale_y_continuous( breaks = c(seq(-1200,-400, by = 400),
                                  seq(0,1200, by=400)),
                      labels = c(seq(-1200,-400, by = 400)* -1,
                                 seq(0,1200, by=400))) 




## Regresiones y resultados


reg1<-lm(ingreso~edad+edad_sqr,data=db_final)

reg2<-lm(ingreso~edad+edad_sqr+niveleduc+as.factor(actividad)+tiempoempresa+tiempoempresa_sqr,data=db_final)


stargazer(reg1,reg2,type="text")




