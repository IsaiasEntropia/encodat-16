pacman::p_load( readr, tidyr, dplyr, janitor, srvyr,
                kableExtra, stringr, foreign, utils)

setwd('C:/Users/ISAIS MORALES/00_conacyt/encodat-16')
pth <- './datos/datos-originales/'

df_hog <- read.csv( paste0(pth, 'ENCODAT_2016_2017_Hogar.csv') , sep= ';' )
df_ind <- readr::read_delim( paste0(pth, 'ENCODAT_2016_2017_Individual.csv'), delim = ';')
#df_integ <- readr::read_delim( paste0(pth, 'ENCODAT_2016_2017_Integrantes.csv'), delim = ';')

# ==== Hogares =========#

df_h1 <-  df_hog %>% group_by(id_hogar) %>% summarise( tothog = n())

sum(df_h1$tothog) # Total de hogares

#df_hog %>% group_by(entidad, munici) %>% summarise(tot = n()) # Hogares por municipios 727 municipios

df_hog$hog  <- 1 

names(df_hog)

df_hog %>% 
  group_by(h203) %>% 
  summarise(tot = n()) # Hogares por municipios 727 municipios

######################

### Crear el disenio muestral

dis_muest <- df_hog %>%
  as_survey_design(ids = code_upm,
                   strata = estrato,
                   weights = ponde_hh)

dis_muest %>% 
  group_by() %>%
  summarise( total = survey_total ( hog ,
                                    vartype = c("cv", "ci"), 
                                    level=0.95)) 

# 35,115,778 # 2020
 
# 32,151,667 # encodat

############ Individual ##############


df_ind <- df_ind %>% mutate( 
  id_hog = substring(id_pers, 1, 20 ) )


df_m <- select(df_hog,id_hogar,   est_var,  ponde_hh,  code_upm,  estrato)

df <-   merge(df_ind, df_m, by.x = 'id_hog'  , by.y = 'id_hogar')

df$estrato

########


df$di1a

df_estima <- df %>% mutate( 
  #di1a = ifelse(di1a == 1, 1, 0),
  di6a = as.integer(di6a),
  di6a =  ifelse(is.na(di6a), 99, di6a),
  #di6a =  ifelse(is.na(di6a), 0, di6a),
  di6a =  ifelse(di6a == 2, 0, di6a),
  di5a = as.integer(di5a),
  di5a =  ifelse(is.na(di5a), 0, di5a),
  di5a_est = ifelse( di5a == 0 , 0, 1),
  di6a_1 = as.integer(di5a_1), 
  di5a_1 =  ifelse(is.na(di5a_1), 0, di5a_1),
  di5a_1 =  ifelse(di5a_1 ==  2, 0, di5a_1),
  di12m = ifelse(di5a_1 == 1 ,  di5a_1, di6a) ,
  pob = 1
) 



dis_muest <- df_estima  %>%
  as_survey_design(ids = code_upm,
                   strata = estrato,
                   weights = ponde_ss)


# Propo

preg <- "di1a"
  
mar_alguna <- dis_muest %>% filter( di1a != 9 ) %>%
  group_by(di1a) %>%
  summarise( prevalencia = survey_prop ( di1a ,
                                         vartype = c("se", "ci"),
                                         level=0.95), proportion = TRUE) 

mar_alguna

# Filtrado únicamente de los que sí han consumido 
mar_alguna <- mar_alguna[2, ]
mar_alguna$prevalencia * 100
mar_alguna$prevalencia_low * 100 
mar_alguna$prevalencia_upp* 100


# TOtal

dis_muest %>% 
  group_by() %>%
  summarise( prevalencia = survey_total ( pob ,
                                          vartype = c("cv", "ci"), level=0.95)) 

pob_low  <- 81738356
pob_punt <- 85233653
pob_upp  <- 88728950


####

df_a <- dis_muest %>% 
  group_by(di5a) %>%
  summarise( prevalencia = survey_total ( di5a ,
                                          vartype = c("se", "ci"), level=0.95)) 

sum(df_a$prevalencia)

df_estima$di5a

mar_alguna <- dis_muest %>%# filter( di6a != 99 ) %>%
  group_by(di1a) %>%
  summarise( prevalencia = survey_prop ( di1a ,
                                          vartype = c("se", "ci"),
                                         level=0.95), proportion = TRUE) 

#mar_alguna

# Filtrado únicamente de los que sí han consumido 
mar_alguna <- mar_alguna[2, ]
mar_alguna$prevalencia * 100
mar_alguna$prevalencia_low * 100 
mar_alguna$prevalencia_upp* 100

2.1 / 100

446/0.021

table(df_estima$di5a)

21,238 # 

table(df_estima$di5a)

1868 +  741 + 501 + 343 + 692 


