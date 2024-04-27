#---#
  #Titulo: Taller R Estadistica y Programacion
  #Nombre: John Medina
  #Codigo: 2202215424
  #RVersion:4.3.2 (2023-10-31 ucrt)
#---#

## instalar/llamar las librerias si se necesitan
  if(!require('pacman')) {
    install.packages('pacman')
    library('pacman')
  }
  
  # función import/export: permite leer/escribir archivos desde diferentes formatos. 
  if(!require('rio')) {
    install.packages('rio')
    library('rio')
  }
  
  # funcion skim: describe un conjunto de datos
  if(!require('skimr')) {
    install.packages('skimr')
    library('skimr')
  }
  
  # contiene conjuntos de datos.
  if(!require('janitor')) {
    install.packages('janitor')
    library('janitor')
  }
  
  # renombar variables
  if(!require('dplyr')) {
    install.packages('dplyr')
    library('dplyr')
  }
  
  # renombar variables
  if(!require('haven')) {
    install.packages('haven')
    library('haven')
  }
  

##Identificar la ruta del directorio
  setwd("~/Semestre 2024-1/John R")
  
##Importar archivo de ubicacion
  location = import(file="Módulo de identificación.dta" , skip=6 , encoding="UTF-8") 
  identification = import(file="Módulo de sitio o ubicación.dta" , skip=6 , encoding="UTF-8") 
  
##Exportar datos en formato .rds
  export(x=location , file="output/location.rds")
  export(x=identification , file="output/identification.rds")
  
##2. Modificación del dataframe de location
  #2.1
  #Inicializamos la comumna
  Location$bussiness_type=NA
  
  #LLenamos la columna
  for (i in Location$GRUPOS4){
    
    info_nueva <- ""
    if (Location$GRUPOS4 == 01){
      info_nueva <- "Agricultura"
    }
    
    if (Location$GRUPOS4 == 02){
      info_nueva <- "Industria manufacturera"
    }
    
    if (Location$GRUPOS4 == 03){
      info_nueva <- "Comercio"
    }
    
    if (Location$GRUPOS4 == 04){
      info_nueva <- "Servicios"
    }
    
    
    Location$bussiness_type[Location$GRUPOS4==i] = info_nueva
    
  }
  
  #2.3
  #Inicializamos la comumna
  identification$ambulante=NA
  
  #LLenamos la columna
  for (i in identification$P3053){
    info_nueva <- 0
    
    if (identification$P3053[i] == 3){
      info_nueva <- 1
    }
    if (identification$P3053[i] == 4){
      info_nueva <- 1
    }
    
    if (identification$P3053[i] == 5){
      info_nueva <- 1
    }
    
    identification$ambulante[identification$P3053==i] = info_nueva
    
  }
  
  ##3. Eliminar filas de un conjunto de datos
  
    #3.1
  identification_sub <- data.frame(identification$DIRECTORIO, identification$SECUENCIA_P,identification$SECUENCIA_ENCUESTA, 
                                   identification$ambulante, identification$COD_DEPTO, identification$F_EXP)
  
    #3.2
  location_sub <- data.frame(Location$DIRECTORIO, Location$SECUENCIA_P,Location$SECUENCIA_ENCUESTA, 
                                    Location$ambulante, Location$COD_DEPTO, Location$F_EXP, identification$P3054, 
                             identification$P469)
  
  
##4. Combinar bases de datos
  
  #4.1"
  Base_combinada <- left_join(x = identification_sub  , y = location_sub, by = c("DIRECTORIO", "SECUENCIA_P","SECUENCIA_ENCUESTA"))

    
##5. Descriptivas
  
  #5.1
  skim(Base_combinada)
  summary(Base_combinada)
  
  #5.2

  variables_descriptivas<- Base_combinada %>% group_by(MES_REF, F_EXP, COD_DEPTO)
  
  total_3034 <- variables_descriptivas %>% summarise(total_3034 = sum(P3034))
  
  # Print the result
  print(total_3034)

  
  
  