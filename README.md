# Problem_set_1_WS_JSV
 
#Autores: Juan Sebastián Vásquez Acevedo y Walter Leonardo Sánchez
#Curso: Big Data and Machine Learning
#PROBLEM SET 1
#Creación: 11/06/2022

# Paquetes necesarios:
	#pacman: este paquete contiene una funcionalidad llamada 'p_load', que permite llamar las librerías que requerimos, sino, intenta instalarlas en la medida de lo posible.
	#tidyverse
	#rvest
	#tibble
	#skimr
	#caret
	#stargazer
	#boot

# Objetivos: 
		1) Extraer de la web una base de datos de la Gran Encuesta Integrada de Hogares (GEIH)- 2018, utilizando técnicas de Web Scraping.
		2) Construir y estimar el mejor modelo de predicción de los ingresos controlando por variables que ayuden esclarecer el comportamiento por género, en las personas ocupadas mayores a 18 años

# El Script:
	Se contruye un script en R que permite llevar a cabo los diferentes objetivos utilizando regresiones, tablas y gráficos.

	En primera instancia, el script desarrolla la técnica de Web Scraping con funciones del paquete "rvest" extrayendo, a través de un proceso iterativo, las particiones de las bases
de la GEIH-2018 alojadas en la página web "https://ignaciomsarmiento.github.io/GEIH2018_sample/", y finalmente, dejar una sola base para uso del proyecto llamada "db". 
Esta base, sufre algunas adecuaciones como creación de varibales o filtros para logra un Data Frame final: db_final.

A partir de la base se estiman modelos de ingresos controlando por variables que a la luz de la literatura, tienen un efecto causal en la tenencia de ingresos. 