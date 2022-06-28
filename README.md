# Problem_set_1_WS_JSV
 
#Autores: Juan Sebastián Vásquez Acevedo y Walter Leonardo Sánchez
#Curso: Big Data and Machine Learning
#PROBLEM SET 1
#Creación: 11/06/2022

# Paquetes necesarios:
	#pacman: este paquete contiene una funcionalidad llamada 'p_load', que permite llamar las librerías que requerimos sino, intenta instalarlas en la medida de lo posible.
	#tidyverse
	#rvest
	#tibble
	#skimr
	#caret
	#stargazer
	#boot

# Objetivos: 
		1) Extraer de la web una base de datos de la Gran Encuesta Integrada de Hogares (GEIH)- 2018, utilizando técnicas de Web Scraping.
		2) Construir y estimar el mejor modelo de predicción de los ingresos controlando por distintas variables que expliquen los ingresos de las personas ocupadas mayores de 18 años

# El Script:
	Se construye un script en R que permite llevar a cabo los diferentes objetivos utilizando regresiones, tablas y gráficos.

En primera instancia, el script desarrolla la técnica de Web Scraping con funciones del paquete "rvest" extrayendo, a través de un proceso iterativo, las particiones de las bases de la GEIH-2018 alojadas en la página web "https://ignaciomsarmiento.github.io/GEIH2018_sample/", y finalmente, dejar una sola base para uso del proyecto llamada "db". 

Esta base sufre algunas adecuaciones como creación de variables o filtros para lograr un Data Frame final: el "db_final".

A partir de la base se estiman modelos de ingresos controlando por variables que a la luz de la literatura, tienen un efecto causal en la tenencia de ingresos. Se calculan diferentes estadísticos como el Laverage, Mean Squart Error (MSE) y Leave-One-Out Cross Validation (LOOCV). Además, se emplean técnicas como el Bootstrap y el Cross-Validation para construir los estadísticos.

Nota!!: Tener en cuenta qué, en las siguientes líneas de código se debe cambiar la ruta por una ruta de directorio del equipo local :
	65,71,189,293,323 y 395

#El Problem Set 1, está compuesto por un repositorio que se compone de 4 carpetas con sus respectivos contenidos en las cuales se encontrará la información necesaria para replicar el documento, de la siguiente manera:
	#Data: Donde se guarda las base del Web Scraping para ser cargada posteriormente.
	#Documentos: Donde se almacena el documento final y algunos archivos de interés.
	#Scripts: Ubicación del script de R llamado "Problem_set_1_JS_WS.R", que contiene toda la codificación para replicar los resultados, tablas y gráficos utilizados en la elaboración del documento.
	#Tablas_graficos: Ruta de almacenamiento de las gráficas y tablas de interés generadas por el script.

