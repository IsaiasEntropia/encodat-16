pacman::p_load( readr, tidyr, dplyr, janitor, srvyr,
                kableExtra, stringr, foreign, utils)

pth <- '/home/isaias/analisis_datos/encodat/datos/originales/'

df_hog <- read.csv( paste0(pth, 'ENCODAT_2016_2017_Hogar.csv') , sep= ';' )
df_int <- read.csv( paste0(pth, 'ENCODAT_2016_2017_Integrantes.csv') , sep= ';' )
df_ind <- read_delim( paste0(pth, 'ENCODAT_2016_2017_Individual.csv'), delim = ';')

### Generales

dim(df_int)

dim(df_hog)
# 55907    48

dim(df_ind)
# 56,877  1715

## Obtener el de de hogar 

df_ind <- df_ind %>% mutate( 
  id_hog = substring(id_pers, 1, 20 ) ) #%>% select(id_hog) 

# Máximo debemos tener a ds personas por hogar
df_ind %>% group_by(id_hog) %>% summarise( tot = n()) %>% arrange( -tot)

## Unir los elementos del diseño muestral

df_m <- select(df_hog,id_hogar,   est_var,  ponde_hh,  code_upm,  estrato)

df_m <-   merge(df_ind, df_m, by.x = 'id_hog'  , by.y = 'id_hogar')

df_m %>% 
  filter( id_hog == '01001000103220120121') %>% 
  select(ponde_ss, ponde_hh, ds3)

## Por diseño muestral ##################

df_estima <- df_m %>% mutate( 
  di6a = as.integer(di6a),
  di6a =  ifelse(is.na(di6a), 0, di6a),
  di5a = as.integer(di5a),
  di5a =  ifelse(is.na(di5a), 0, di5a),
  di5a_est = ifelse( di5a == 0 , 0, 1),
  di5a_1 = as.integer(di5a_1), 
  di5a_1 =  ifelse(is.na(di5a_1), 0, di5a_1),
  di12m = ifelse(di5a_1 == 1 ,  di5a_1, di6a) ,
  pob = 1
) 

df_estima$di6a

df_estima$di8a

# Pruebas d eque todo se calculó bien

df_estima %>% select(di5a_1, di6a, di12m) %>% 
  filter( di12m != 0   )

### Crear el diseño muestral

dis_muest <- df_estima %>%
  as_survey_design(ids=code_upm,
                   strata= estrato,
                   weights=ponde_ss)

## Calcular el indicador 1

# Población
dis_muest %>% 
  group_by() %>%
  summarise( prevalencia = survey_total ( pob ,
                                          vartype = c("cv", "ci"), level=0.95)) 

##  Prevalencia mariguana algna vez en la vida
dis_muest %>% 
  group_by(di5a_est) %>%
  summarise( prevalencia = survey_total ( di5a_est ,
                                          vartype = c("se", "ci"), level=0.95)) 
# Utilizar la pregunta DI5-A

6867286 / 85233653 * 100      
7372102 / 85233653 * 100 # Es con la pregunta DI5-A para mariguana. Ya que sería los valores que no 
7876918 / 85233653 * 100

# 8.184| 8.6 |9.114

##  Prevalencia mariguana en los últmos 12 meses, se debe considerar la respuesta de la DI5_1 y DI6

dis_muest %>% 
  group_by(di12m) %>%
  summarise( prevalencia = survey_total ( di12m ,
                                          vartype = c("se", "ci"), level=0.95)) 
1619911 / 85233653 * 100
1821085 / 85233653 * 100
2022259 / 85233653 * 100

# 1.911| 2.1 | 2.362









### Por cálculo manual ###############

### Cálculo de prevalencia

df_m %>% group_by(di1a) %>% summarise(tot = n())

# 4217/56877 = 0.74 # Esto es sin ponderadores

# 1  4217
# 2 52417
# 9   243


df_ind$
  df_mari = df_m %>% 
  #filter( di6a == 1) %>%
  mutate ( di1a_int = case_when( di1a == 1 ~ 1,
                                 di1a == 2 ~ 0,
                                 di1a == 9 ~ 0),
           di1a_int = di1a_int * ponde_ss, 
           di6a_int = case_when( 
             di6a == 1 ~ 1, 
             di6a == 2 ~ 0),
           di6a_int =  ifelse(is.na(di6a_int), 0, di6a_int),
           di6a_int = di6a_int * ponde_ss
  ) %>% 
  select(di1a_int, di6a_int, ponde_ss, ds2 )

## Resultados ###
## Drogas ilegales
# Marijuana

# Población total entre 15 y 65 años
sum(df_mari$ponde_ss)
sum(df_mari$di1a_int)
sum(df_mari$di6a_int)

# 20,439,584,531,418,763,264 resultado de población?
# 85,233,653 Ponderador 
# 120, 000, 000 población 2016 aprox

# Ya da 8.8 %

(sum(df_mari$di1a_int) / sum(df_mari$ponde_ss) )* 100

# Cálculo

7502491/85233653 * 100
6694491/85233653 * 100
808000/85233653 * 100
