pacman::p_load( readr, tidyr, dplyr, janitor, srvyr,
                kableExtra, stringr, foreign, utils)

setwd('C:/Users/ISAIS MORALES/00_conacyt/encodat-16')
pth <- './datos/datos-originales/'

df_hog <- read.csv( paste0(pth, 'ENCODAT_2016_2017_Hogar.csv') , sep= ';' )
df_ind <- readr::read_delim( paste0(pth, 'ENCODAT_2016_2017_Individual.csv'), delim = ';')

### Generales
dim(df_hog)
# 55907    48
dim(df_ind)
# 56,877  1715

df_hog %>% 
  select (id_hogar, entidad,  desc_ent, munici, desc_mun, locali, desc_loc, code_upm,estrato, est_var, ponde_hh) %>%
  head(5)

## Obtener el de hogar. 

df_ind <- df_ind %>% mutate( id_hog = substring(id_pers, 1, 20 ) ) #%>% select(id_hog) 

df_ind$id_pers

hist(df_ind$ponde_ss)

summary(df_ind$ponde_ss)


# Máximo debemos tener a ds personas por hogar

df_ind %>% group_by(id_hog) %>% summarise( tot = n() ) %>% arrange( -tot)

## Unir los elementos del diseño muestral

df_m <- select(df_hog, id_hogar,   est_var,  ponde_hh,  code_upm,  estrato)

df_m <-   merge(df_ind, df_m, by.x = 'id_hog'  , by.y = 'id_hogar')

# Probamos que la edad es la misma

df_m %>% 
  filter( id_hog == '01001000103220120121') %>% 
  select(id_hog, id_pers) 

df_m %>% 
  filter( id_hog == '01001000103220050011') 

## Por disenio muestral ##################

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
pob_ent <- dis_muest %>% 
              group_by(entidad) %>%
              summarise( total = survey_total ( pob ,
                                                      vartype = c("cv", "ci"), level=0.95)) 

##  Prevalencia mariguana algna vez en la vida
prev_mari <- dis_muest %>% 
                group_by(entidad, di5a_est) %>%
                summarise( prevalencia = survey_total ( di5a_est ,
                                                        vartype = c("se", "ci"), level=0.95)) %>%
  filter (di5a_est != 0 )

prev_mari$prevalencia / pob_ent$total


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


(sum(df_mari$di1a_int) / sum(df_mari$ponde_ss) )* 100

# Cálculo

7502491/85233653 * 100
6694491/85233653 * 100
808000/85233653 * 100
