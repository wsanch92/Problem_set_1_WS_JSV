#Autores: Juan Sebastián Vásquez Acevedo y Walter Leonardo Sánchez
#Curso: Big Data and Machine Learning
#PROBLEM SET 1
#Creación: 11/06/2022



## Instalar packages
#install.packages("XML")

##llamar packages
require(pacman)
p_load(tidyverse, rvest, tibble,rio,skimr,caret,stargazer, boot)



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

#pc Walter
#db <- read_rds("C:/Users/walte/OneDrive/Documentos/Maestría en Economía Aplicada/Big Data/GitHub/Talleres/Problem_set_1_WS_JSV/Data/Datos_geih_bogota.rds")

#pc juancho

db<- read_rds("/Users/usuario/Desktop/Problem_set_1_WS_JSV/Data/Datos_geih_bogota.rds")

## Creación y tratamiento de variables
## ocupados  mayores de 18

db_1 <- db[db$age >= 18 & db$ocu == 1, ]
colnames(db_1)

##Análisis de ingresos totales

db_1[db_1$ingreso==0,]$ingtotes

variables_ingreso <- c("ingtot", "ingtotes", "ingtotob", "iof1", "iof1es", "iof2", "iof2es", "iof3h", "iof3hes", "iof3i", "iof3ies", "iof6", "iof6es", "isa", "isaes", "ie", "iees", "imdi", "imdies", "impa", "impaes", "p6240", "ocu", "relab")
j_1<- db_1[db_1$ingtot==0,variables_ingreso]
#View(j_1)
#skim(j_1)
##Luego de indentificar que las observaciones que tienen un ingreso total en 0 y que esto se mantienen aún luego de la imputación decidimos borrarlas, esto corresponde a 265 observaciones.
db_1<- db_1[db_1$ingtot>0,]
#skim(db_1$ingtot)


## Creación de lista con las variables elegibles
variables <- c('age','ingtot','ingtotes','ingtotob','p6210','p6210s1','p6240','relab','sex','fex_c','mes','p6426','oficio')
##creacion de lista y base
db_final <- db_1[,variables]  ## Se filtra la base por la lista de variables elegibles
db_final%>%head()
colnames(db_final)


## Renombrar variables
names (db_final) <- c('edad','ingreso','ingtotes','ingtotob','niveleduc','p6210s1','actividad','ocupacion','sex','fex_c','mes','tiempoempresa','oficio')

db_final<- db_final%>%mutate(ocupacion=ifelse(ocupacion==8,
                                          9,
                                          ocupacion))



## Creación de lista de variables elegidas
vars_final <- c('edad','ingreso','niveleduc','actividad','ocupacion','sex','tiempoempresa','oficio')

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



#ggplot() + geom_histogram(data=db_1 , aes(x=y_total_m) , fill="coral1" , alpha=0.5) +
#geom_histogram(data=db_1 , aes(x=ingtotob) , fill="blue" , alpha=0.5)



############################ Tablas y summarice  ############################


## Resumen de la base con las variables elegidas

skim(db_final[,vars_final])
table(db_final$ocupacion)

## Gráficos

#ggplot() + geom_histogram(data=db_1 , aes(x=ingtot) , fill="coral1" , alpha=0.5) + 
  #geom_histogram(data=db_1 , aes(x=ingtotes) , fill="blue" , alpha=0.5) +
  #geom_histogram(data=db_1 , aes(x=ingtotob) , fill="red" , alpha=0.5)
#

#ggplot() + geom_histogram(data=db_1 , aes(x=ingtot) , fill="coral1" , alpha=0.5) + 
  #geom_histogram(data=db_1 , aes(x=ingtotob) , fill="blue1" , alpha=0.5)

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



##Regresiones y resultados

#Revisar las dos vareiables de ocupación porque una contiene los ocupados más la PET o PEA
reg1<-lm(ingreso~edad+edad_sqr,data=db_final)
reg2<-lm(ingreso~edad+edad_sqr+niveleduc,data=db_final)
reg3<-lm(ingreso~edad+edad_sqr+niveleduc+tiempoempresa,data=db_final)
reg4<-lm(ingreso~edad+edad_sqr+niveleduc+tiempoempresa+tiempoempresa_sqr,data=db_final)
## debido a que el signo del coeficiente no es el esperado según la teoria, planteamos un modelo en el cual solo incluimos las personas que se encuentran trabajando
reg5<-lm(ingreso~edad+edad_sqr+niveleduc+tiempoempresa+tiempoempresa_sqr, factor(ocupacion),data=db_final)
reg5.1<-lm(ingreso~edad+edad_sqr+niveleduc+tiempoempresa+tiempoempresa_sqr, factor(ocupacion),data=db_final[db_final$actividad==1,])
reg5.2<-lm(ingreso~edad+edad_sqr+niveleduc+tiempoempresa+tiempoempresa_sqr, factor(oficio),data=db_final)
reg5.3<-lm(ingreso~edad+edad_sqr+niveleduc+tiempoempresa+tiempoempresa_sqr, factor(oficio),data=db_final[db_final$actividad==1,])




stargazer(reg1,reg2,reg3,reg4,reg5,reg5.1,reg5.2,reg5.3,type="text",out = "/Users/usuario/Desktop/Problem_set_1_WS_JSV/Data/regresiones.htm")
##modelos definitivos a presentar 
stargazer(reg1,reg4,type="text",out = "/Users/usuario/Desktop/Problem_set_1_WS_JSV/Data/regresiones_presentar.htm")

#?stargazer

plot(db_final$edad,db_final$ingreso,xlab = 'Edad', ylab='Ingreso',cex=0.5)
abline(reg4, col = "red")


#Predicciones

db_final$yhat_reg1 <-predict(reg1,db_final)
yhat_reg1<-predict(reg1,db_final)
mse_1<- mean((db_final$ingreso-yhat_reg1)^2)

db_final$yhat_reg2 <-predict(reg2)
yhat_reg2<-predict(reg2,db_final)
mse_2<- mean((db_final$ingreso-yhat_reg2)^2)

db_final$yhat_reg3 <-predict(reg3)
yhat_reg3<-predict(reg3,db_final)
mse_3<- mean((db_final$ingreso-yhat_reg3)^2)

db_final$yhat_reg4 <-predict(reg4)
yhat_reg4<-predict(reg4,db_final)
mse_4<- mean((db_final$ingreso-yhat_reg4)^2)
mse_4
##gráfica de los predichos del modelo 
plot(db_final$edad,db_final$yhat_reg4,xlab = 'Edad', ylab='Ingreso',cex=0.5)
abline(reg4, col = "red")



db_final$yhat_reg5 <-predict(reg5)
yhat_reg5<-predict(reg5,db_final)
mse_5<- mean((db_final$ingreso-yhat_reg5)^2)

db_final[db_final$actividad==1,]$yhat_reg5.1 <-predict(reg5.1)
yhat_reg5.1<-predict(reg5.1,db_final)
mse_5.1<- mean((db_final$ingreso-yhat_reg5.1)^2)

db_final$yhat_reg5.2 <-predict(reg5.2)
yhat_reg5.2<-predict(reg5.2,db_final)
mse_5.2<- mean((db_final$ingreso-yhat_reg5.2)^2)

db_final[db_final$actividad==1,]$yhat_reg5.3 <-predict(reg5.3)
yhat_reg5.3<-predict(reg5.3,db_final)
mse_5.3<- mean((db_final$ingreso-yhat_reg5.3)^2)

mse_f<-c(mse_1,mse_2,mse_3,mse_4,mse_5,mse_5.1,mse_5.2,mse_5.3)
mse_f


## grafico de ingreso predicho por edad
ggplot() + 
  geom_col(db_final , mapping = aes(x = edad, y = yhat_reg1, fill="reg1")) +
  geom_col(db_final , mapping = aes(x = edad, y =  yhat_reg2, fill="reg2")) + 
  geom_col(db_final , mapping = aes(x = edad, y =  yhat_reg3, fill="reg3")) +
  geom_col(db_final , mapping = aes(x = edad, y =  yhat_reg4, fill="reg4")) +
  geom_col(db_final , mapping = aes(x = edad, y =  yhat_reg5, fill="reg5")) +
  geom_col(db_final , mapping = aes(x = edad, y =  yhat_reg5.1, fill="reg5.1")) +
  geom_col(db_final , mapping = aes(x = edad, y =  yhat_reg5.2, fill="reg5.2")) +
  geom_col(db_final , mapping = aes(x = edad, y =  yhat_reg5.3, fill="reg5.3"))           

## Grafico ingresos predicho por edad modelo 4 
ggplot() +
  geom_line(db_final , mapping = aes(x = edad, y =  yhat_reg4, fill="reg4")) +
  geom_line(db_final, mapping = aes(x = edad, y = ingreso, fill="ingreso"))
  

## Bootstrap
set.seed(2022)


prueba_boot.fn<-function(db_final,index){
  ## el index se utilizará para obtener los pesos para el bootstrap
  coef(lm(ingreso~edad+edad_sqr+niveleduc+tiempoempresa+tiempoempresa_sqr,data=db_final, subset = index))
}

boot<-boot(db_final, prueba_boot.fn, R=1000)
boot

reg4
stargazer(reg4, type="text")

## recordar calculo en excel
#CI=[coef−1.96×SE,coef+1.96×SE]

## derevida en excel para calcular edad pico (mayor ingreso)


############# PUNTO CUARTO ######################

## creación variable dummy de sexo 

db_final<- db_final%>%mutate(mujer=ifelse(sex==1,
                                          0,
                                          1))
db_final<- db_final%>%mutate(log_ingreso=log(ingreso))

## Primer modelo diferencia de ingresos por sexo
mod1<-lm(log_ingreso~mujer,data=db_final)
mod1
stargazer(mod1,type="text",out = "/Users/usuario/Desktop/Problem_set_1_WS_JSV/Data/mod1_por_sexo.htm")



## qué tan bueno es el modelo en la muestra
db_final$yhat_mod1 <-predict(mod1)
yhat_mod1<-predict(mod1,db_final)
mse_mod1<- mean((db_final$log_ingreso-yhat_mod1)^2)
mse_mod1



#partir en dos subset por hombre y por mujer y correr las regresiones, estimar y pointar los predichos

db_mujer <- db_final[db_final$sex == 0, ]
db_hombre <- db_final[db_final$sex == 1, ]

## Modelo con base exclusiva para mujeres
mod_m<-lm(log_ingreso~edad+edad_sqr+niveleduc+tiempoempresa+tiempoempresa_sqr,data=db_mujer)
mod_m
## Modelo con base exclusiva para hombres
mod_h<-lm(log_ingreso~edad+edad_sqr+niveleduc+tiempoempresa+tiempoempresa_sqr,data=db_hombre)
mod_h

## Gráfico de modelo por genero 
plot(db_final$edad,db_final$log_ingreso,xlab = 'Edad', ylab='log_ingreso',cex=0.5)
abline(mod_m, col = "red")
abline(mod_h, col = "blue")

## Comprar modelos y exportarlos
stargazer(mod_m,mod_h,type="text",out = "/Users/usuario/Desktop/Problem_set_1_WS_JSV/Data/regresiones_por_sexo.htm")

##recordar hacer la derivada para los modelos por sexo

##bootstrap modelos por sexo 
set.seed(2022)


prueba_boot.fn<-function(db_mujer,index){
  ## el index se utilizará para obtener los pesos para el bootstrap
  coef(lm(log_ingreso~edad+edad_sqr+niveleduc+tiempoempresa+tiempoempresa_sqr,data=db_mujer, subset = index))
}

boot_m<-boot(db_mujer, prueba_boot.fn, R=1000)
boot_m

mod_m
stargazer(mod_m, type="text")

##hombre

set.seed(2022)


prueba_boot.fn<-function(db_hombre,index){
  ## el index se utilizará para obtener los pesos para el bootstrap
  coef(lm(log_ingreso~edad+edad_sqr+niveleduc+tiempoempresa+tiempoempresa_sqr,data=db_hombre, subset = index))
}

boot_h<-boot(db_hombre, prueba_boot.fn, R=1000)
boot_h

mod_h
stargazer(mod_h, type="text")

## recordar construir los intervalos de confianza en Excel


##controlar por ocupación para ver como se reduce la brecha y establecer la explicación de porque se disminuye dicha brecha

mod2<-lm(log_ingreso~mujer+as.factor(ocupacion),data=db_final)## primer modelo y~x
mod2
mod1

##teorema FWL
mod2
db_final<- db_final%>%mutate(ej=c(rep(0,nrow(db_final)-1),1))
tail(db_final)

##regresión con la variable ej 
mod3<-lm(log_ingreso~mujer+ej,data=db_final) ## regresión con la dummy
mod3

db_final<-db_final %>% mutate(res_y_e=lm(log_ingreso~ej,db_final)$residuals,
                              res_x_e=lm(mujer~ej,db)$residuals,
)

mod4<-lm(res_y_e~res_x_e,db_final)

stargazer(mod1,mod3,mod4,type="text")

## qué tan bueno controlando por la empleabilidad en el modelo por sexo
db_final$yhat_mod2 <-predict(mod2)
yhat_mod2<-predict(mod2,db_final)
mse_mod2<- mean((db_final$log_ingreso-yhat_mod2)^2)
mse_mod2


############# PUNTO QUINTO ######################
#luego de hacer train y tren,  y después MSE error deben ir disminuyendo hasta un punto y luego subir, esto con cada partición de la muestra

###################################################################
## DATOS ATÍPICOS
Q<- quantile(db_final$log_ingreso,probs = c(0.25,0.75),na.rm=F)
iqr <- IQR(x=db_final$log_ingreso , na.rm=F)
up <- Q[2]+1.5*iqr # Upper Range
low<- Q[1]-1.5*iqr # Lower Range
###################################################################

set.seed(10101)

## Creación de las bases de entrenamiento y prueba


db_final<- db_final%>%mutate(edad_3=edad^3,
                             edad_4=edad^4,
                             tiempoempresa_3=tiempoempresa^3,
                             tiempoempresa_4=tiempoempresa^4,
                             mujer_empresa=mujer*tiempoempresa,
                             )


id_entrenamiento <- sample(1:nrow(db_final),size = 0.7*nrow(db_final),replace=F)
entrenamiento <- db_final[id_entrenamiento,]
prueba<- db_final[-id_entrenamiento,]

## Modelo de referencia
mod_ref <- lm(log_ingreso~1,data=entrenamiento)

y_hat_dentro_mf <- exp(predict(mod_ref, entrenamiento))
y_hat_fuera_mf <-exp(predict(mod_ref, prueba))
y_real_dentro_mf <- entrenamiento$ingreso
y_real_fuera_mf <- prueba$ingreso

mse_dentro_mf <- mean((y_real_dentro_mf-y_hat_dentro_mf)^2)
mse_fuera_mf <- mean((y_real_fuera_mf-y_hat_fuera_mf)^2)

mse_fuera_mf/mse_dentro_mf ### ver el overfitting


## Modelo por sexo

mod_sex <- lm(log_ingreso~mujer,data=entrenamiento)

y_hat_dentro_sex <- exp(predict(mod_sex, entrenamiento))
y_hat_fuera_sex <-exp(predict(mod_sex, prueba))
y_real_dentro_sex <- entrenamiento$ingreso
y_real_fuera_sex <- prueba$ingreso

mse_dentro_sex <- mean((y_real_dentro_sex-y_hat_dentro_sex)^2)
mse_fuera_sex <- mean((y_real_fuera_sex-y_hat_fuera_sex)^2)

mse_fuera_sex/mse_dentro_sex ### ver el overfitting

## Modelo por sexo y edad

mod_sex_edad <- lm(log_ingreso~mujer+edad+edad_sqr,data=entrenamiento)

y_hat_dentro_sex_edad <- exp(predict(mod_sex_edad, entrenamiento))
y_hat_fuera_sex_edad <-exp(predict(mod_sex_edad, prueba))
y_real_dentro_sex_edad <- entrenamiento$ingreso
y_real_fuera_sex_edad <- prueba$ingreso

mse_dentro_sex_edad <- mean((y_real_dentro_sex_edad-y_hat_dentro_sex_edad)^2)
mse_fuera_sex_edad <- mean((y_real_fuera_sex_edad-y_hat_fuera_sex_edad)^2)

mse_fuera_sex_edad/mse_dentro_sex_edad

##A.3
## Modelo por sexo  - edad y ocupación

mod_sex_edad_ocu <- lm(log_ingreso~mujer+edad+edad_sqr+as.factor(ocupacion),data=entrenamiento)

y_hat_dentro_sex_edad_ocu <- exp(predict(mod_sex_edad_ocu, entrenamiento))
y_hat_fuera_sex_edad_ocu <-exp(predict(mod_sex_edad_ocu, prueba))
y_real_dentro_sex_edad_ocu <- entrenamiento$ingreso
y_real_fuera_sex_edad_ocu <- prueba$ingreso
mse_dentro_sex_edad_ocu <- mean((y_real_dentro_sex_edad_ocu-y_hat_dentro_sex_edad_ocu)^2)
mse_dentro_sex_edad_ocu
mse_fuera_sex_edad_ocu <- mean((y_real_fuera_sex_edad_ocu-y_hat_fuera_sex_edad_ocu)^2)
mse_fuera_sex_edad_ocu

mse_fuera_sex_edad_ocu/mse_dentro_sex_edad_ocu

## Modelo por sexo  - edad3 y ocupación

mod_sex_edad3 <- lm(log_ingreso~mujer+edad+edad_sqr+as.factor(ocupacion)+edad_3,data=entrenamiento)

y_hat_dentro_sex_edad3 <- exp(predict(mod_sex_edad3, entrenamiento))
y_hat_fuera_sex_edad3 <-exp(predict(mod_sex_edad3, prueba))
y_real_dentro_sex_edad3 <- entrenamiento$ingreso
y_real_fuera_sex_edad3 <- prueba$ingreso

mse_dentro_sex_edad3 <- mean((y_real_dentro_sex_edad3-y_hat_dentro_sex_edad3)^2)
mse_dentro_sex_edad3
mse_fuera_sex_edad3 <- mean((y_real_fuera_sex_edad3-y_hat_fuera_sex_edad3)^2)
mse_fuera_sex_edad3

mse_fuera_sex_edad3/mse_dentro_sex_edad3

## Modelo por sexo - tiempoempresa3

mod_sex_empresa3 <- lm(log_ingreso~mujer+edad+edad_sqr+as.factor(ocupacion)+edad_3+tiempoempresa+tiempoempresa_sqr+tiempoempresa_3,data=entrenamiento)

y_hat_dentro_sex_empresa3 <- exp(predict(mod_sex_empresa3, entrenamiento))
y_hat_fuera_sex_empresa3 <-exp(predict(mod_sex_empresa3, prueba))
y_real_dentro_sex_empresa3 <- entrenamiento$ingreso
y_real_fuera_sex_empresa3 <- prueba$ingreso

mse_dentro_sex_empresa3 <- mean((y_real_dentro_sex_empresa3-y_hat_dentro_sex_empresa3)^2)
mse_fuera_sex_empresa3 <- mean((y_real_fuera_sex_empresa3-y_hat_fuera_sex_empresa3)^2)

mse_fuera_sex_empresa3/mse_dentro_sex_empresa3

## Modelo REG4 

mod_reg4 <- lm(ingreso~edad+edad_sqr+niveleduc+tiempoempresa+tiempoempresa_sqr,data=entrenamiento)

y_hat_dentro_reg4 <- predict(mod_reg4, entrenamiento)
y_hat_fuera_reg4 <-predict(mod_reg4, newdata=prueba)
y_real_dentro_reg4 <- entrenamiento$ingreso
y_real_fuera_reg4 <- prueba$ingreso

mse_dentro_reg4 <- mean((y_real_dentro_reg4-y_hat_dentro_reg4)^2)
mse_fuera_reg4 <- mean((y_real_fuera_reg4-y_hat_fuera_reg4)^2)

mse_fuera_reg4/mse_dentro_reg4 

## Modelo edad

mod_edad <- lm(ingreso~edad+edad_sqr,data=entrenamiento)

y_hat_dentro_edad <- predict(mod_edad, entrenamiento)
y_hat_fuera_edad <-predict(mod_edad, prueba)
y_real_dentro_edad <- entrenamiento$ingreso
y_real_fuera_edad <- prueba$ingreso

mse_dentro_edad <- mean((y_real_dentro_edad-y_hat_dentro_edad)^2)
mse_fuera_edad <- mean((y_real_fuera_edad-y_hat_fuera_edad)^2)

mse_fuera_edad/mse_dentro_edad ### ver el overfitting

## modelo por sexo - edad4

mod_sex_edad4 <- lm(log_ingreso~mujer+edad+edad_sqr+edad_3+edad_4+as.factor(ocupacion)+tiempoempresa+tiempoempresa_sqr+tiempoempresa_3,data=entrenamiento)

y_hat_dentro_sex_edad4 <- exp(predict(mod_sex_edad4, entrenamiento))
y_hat_fuera_sex_edad4 <-exp(predict(mod_sex_edad4, prueba))
y_real_dentro_sex_edad4 <- entrenamiento$ingreso
y_real_fuera_sex_edad4 <- prueba$ingreso

mse_dentro_sex_edad4 <- mean((y_real_dentro_sex_edad4-y_hat_dentro_sex_edad4)^2)
mse_fuera_sex_edad4 <- mean((y_real_fuera_sex_edad4-y_hat_fuera_sex_edad4)^2)

mse_fuera_sex_edad4/mse_dentro_sex_edad4

## modelo por sexo - tiempo empresa 4

mod_sex_empresa4 <- lm(log_ingreso~mujer+edad+edad_sqr+edad_3+edad_4+as.factor(ocupacion)+tiempoempresa+tiempoempresa_sqr+tiempoempresa_3+tiempoempresa_4,data=entrenamiento)

y_hat_dentro_sex_empresa4 <- exp(predict(mod_sex_empresa4, entrenamiento))
y_hat_fuera_sex_empresa4 <-exp(predict(mod_sex_empresa4, prueba))
y_real_dentro_sex_empresa4 <- entrenamiento$ingreso
y_real_fuera_sex_empresa4 <- prueba$ingreso

mse_dentro_sex_empresa4 <- mean((y_real_dentro_sex_empresa4-y_hat_dentro_sex_empresa4)^2)
mse_fuera_sex_empresa4 <- mean((y_real_fuera_sex_empresa4-y_hat_fuera_sex_empresa4)^2)

mse_fuera_sex_empresa4/mse_dentro_sex_empresa4


#### gráfica de los modelos por MSE en Prueba (Test)
mse <- c(mse_fuera_mf, mse_fuera_sex, mse_fuera_sex_edad, mse_fuera_sex_edad_ocu, mse_fuera_sex_edad3, mse_fuera_sex_empresa3, mse_fuera_reg4, mse_fuera_edad, mse_fuera_sex_edad4, mse_fuera_sex_empresa4)
ScaleXmod<-c('m1', 'm2', 'm3' ,'m4', 'm5','m6','m7','m8','m9','m10')
grafica_mse <- data.frame(modelos=c(1,2,3,4,5,6,7,8,9,10),
                          MSE=mse)
rownames(grafica_mse)<-c(1,2,3,4,5,6,7,8,9,10)

ggplot(grafica_mse, aes(x=modelos,y=MSE, group = 1)) + 
  geom_line() +
  theme_classic() + 
  scale_x_continuous(breaks = c(1:10),
                     labels= ScaleXmod)


#### Punto 5. a. v)

# Se calcula el Laverage statistic para la muestra de test

#Loop para cada observación

test <- prueba       ## clonamos la base             
rownames(test) <- 1:nrow(test)  # resetear el índice
nrow(test)

N_t<- nrow(test)
##N_t<-3 testeo para el loop

## Iteración sobre la cantidad de filas de la muestra de prueba para clcular el laverage statistic (alpha)
laverages <- c()
for (j_h in 1:N_t){
  test<-test %>% mutate(res_y_x = lm(ingreso~edad+edad_sqr+niveleduc+tiempoempresa+tiempoempresa_sqr,test)$residuals,
                            e_j = ifelse(rownames(test)==j_h,
                                              1,0),
                            res_e_x=lm(e_j~edad+edad_sqr+niveleduc+tiempoempresa+tiempoempresa_sqr,test)$residuals,
  )
  reg4<-lm(res_y_x~res_e_x,test)
  u_j<-lm(ingreso~edad+edad_sqr+niveleduc+tiempoempresa+tiempoempresa_sqr,data=test)$residual[j_h]
  h_j<-lm.influence(reg1)$hat[j_h]
  alpha<-u_j/(1-h_j)
  laverages <- c(laverages,alpha)
}


length(laverages) ## comprobar el largo de laverage para ver si tiene el mismo número de filas que la muestra de copia test
prueba$laverage <- laverages ## pegar al dataframe de prueba los laverages
prueba <- prueba %>% mutate(atipico = ifelse((log_ingreso>up | log_ingreso< low),
                                             1,
                                             0)) ## crear variable que marca si es atípico o no
## Ordenar la muestra de prueba para ver si los laverages mayores tienen datos atípicos
prueba <- prueba[order(prueba$laverage,decreasing = T),]




#### Punto 5. b)


set.seed(101010)

### falta el modelo de la constante y el LAverage

modelos <- list(log_ingreso~mujer,
             log_ingreso~mujer+edad+edad_sqr,
             log_ingreso~mujer+edad+edad_sqr+as.factor(ocupacion),
             log_ingreso~mujer+edad+edad_sqr+as.factor(ocupacion)+edad_3,
             log_ingreso~mujer+edad+edad_sqr+as.factor(ocupacion)+edad_3+tiempoempresa+tiempoempresa_sqr+tiempoempresa_3,
             ingreso~edad+edad_sqr+niveleduc+tiempoempresa+tiempoempresa_sqr,
             ingreso~edad+edad_sqr,
             log_ingreso~mujer+edad+edad_sqr+edad_3+edad_4+as.factor(ocupacion)+tiempoempresa+tiempoempresa_sqr+tiempoempresa_3,
             log_ingreso~mujer+edad+edad_sqr+edad_3+edad_4+as.factor(ocupacion)+tiempoempresa+tiempoempresa_sqr+tiempoempresa_3+tiempoempresa_4)

MSE_CV <- c()
for (i in modelos){
  model<-train(i,
               data = db_final[,-3],
               trControl = trainControl(method = "cv", number = 5),
               method = "null")                                            # specifying regression model
  
  mse_cv <- model[["results"]][["RMSE"]]
  MSE_CV<-c(MSE_CV,mse_cv)
}
MSE_CV
MSE_CV_2<-c(log(MSE_CV[6]))
MSE_CV_2 #### Las unidades de medida están generando problemática








#### Punto 5. c)
# LOOCV

N<- nrow(db_final)


## Iteración sobre la cantidad de filas de la muestra de prueba para clcular el laverage statistic (alpha)

for (i_h in 1:N){
  reg_loocv<-lm(ingreso~edad+edad_sqr+niveleduc+tiempoempresa+tiempoempresa_sqr, data=db_final[-i_h,])
                y_hat_i <- predict(reg_loocv, db_final[i_h,])
                error_predict <- db_final[i_h,]$ingreso-y_hat_i
  LOOCv <- mean(error_predict)
}
LOOCv
