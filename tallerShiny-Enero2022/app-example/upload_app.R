#------------------------------------------------------------------------------#
# Proyect title: "Observatorio de Cohesion Social" 
# Author(s): Julio Iturra Sanhueza & Juan Carlos Castillo
# website: https://www.linkedin.com/in/jciturras/
# e-mail: julioiturrasanhueza@gmail.com
#------------------------------------------------------------------------------#

#********************************
#Upload app *********************
#********************************

#Este archivo sirve principalmente para generar el deploy de la app en el servidor de shiny.apps


# install.packages('rsconnect')
# install.packages('here')
# install.packages("tictoc") #timming de funciones


library(rsconnect)
library(here)


# Realizar el login de la cuenta y el token de acceso  --------------------

rsconnect::setAccountInfo(name='su_cuenta', #account
                          token='codigo-dash',
                          secret='secret')

tictoc::tic() #inicio...
rsconnect::deployApp(forceUpdate = TRUE) #Subir App al server de shinyapps
tictoc::toc() #término...

