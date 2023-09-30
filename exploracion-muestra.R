pacman::p_load( readr, tidyr, dplyr, janitor, srvyr,
                kableExtra, stringr, foreign, utils)

setwd('C:/Users/ISAIS MORALES/00_conacyt/encodat-16')
pth <- './datos/datos-originales/'

df_hog <- read.csv( paste0(pth, 'ENCODAT_2016_2017_Hogar.csv') , sep= ';' )
df_ind <- readr::read_delim( paste0(pth, 'ENCODAT_2016_2017_Individual.csv'), delim = ';')
df_integ <- readr::read_delim( paste0(pth, 'ENCODAT_2016_2017_Integrantes.csv'), delim = ';')

####  Preguntas 

# Sexo

df_ind %>% group_by(ds2) %>% summarise(t = n(), 
                                        prop = (t/56889)*100) 
# Fecha 

boxplot(df_ind$ds3)

# Grado escolar
df_ind %>% group_by(ds9) %>% summarise(t = n(), 
                                        prop = (t/56889)*100) 
# Fecha 
boxplot(df_ind $f2)
df_ind %>% group_by(ds3) %>% summarise(t = n(), prop = (t/56889)*100) 

# Fecha 
boxplot(df_ind $f2)
df_ind %>% group_by(ds3) %>% summarise(t = n(), prop = (t/56889)*100) 

