pacman::p_load( readr, tidyr, dplyr, janitor, srvyr,
                kableExtra, stringr, foreign, utils)

pth <- './datos/datos-originales/'


df_hog <- read.csv( paste0(pth, 'ENCODAT_2016_2017_Hogar.csv') , sep= ';' )
df_int <- read.csv( paste0(pth, 'ENCODAT_2016_2017_Integrantes.csv') , sep= ';' )
df_ind <- read_delim( paste0(pth, 'ENCODAT_2016_2017_Individual.csv'), delim = ';')

## Obtener el de de hogar 

df_ind <- df_ind %>% mutate( 
  id_hog = substring(id_pers, 1, 20 ) ) #%>% select(id_hog) 

# Máximo debemos tener a ds personas por hogar
df_ind %>% group_by(id_hog) %>% summarise( tot = n()) %>% arrange( -tot)

## Unir los elementos del diseño muestral

df_m <- select(df_hog,id_hogar,   est_var,  ponde_hh,  code_upm,  estrato)

df_m <-   merge(df_ind, df_m, by.x = 'id_hog'  , by.y = 'id_hogar')


# tb01 -  Si dice que Sí ha fumado alguna vez, poner 1
# Si tb01 es no, revisar que tb02

df_estima %>% select(tb01,tb02, tb03, tb04,  tb05) #%>%
filter(tb05 == 1)

## Por diseño muestral ##################

df_estima <- df_m %>% mutate( 
  pob= 1, 
  tb05 = as.integer(tb05),
  tb05 =  ifelse(is.na(tb05), 0, tb05),
  tabaco = ifelse(tb05 == 1, 1, tb01),
  factual = ifelse( tb02 %in% 1:2, 1, 0 )) 

df_estima %>% select(tb01,tb02, tb03, tb04,  tb05) #%>%


# Pruebas d eque todo se calculó bien
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

##  Fumadorxs diarios
dis_muest %>% 
  group_by(factual) %>%
  summarise( prevalencia = survey_total ( factual,
                                          vartype = c("se", "ci"), level=0.95)) 