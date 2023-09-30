pacman::p_load( readr, tidyr, dplyr, janitor, srvyr,
                kableExtra, stringr, foreign, utils)

setwd('C:/Users/ISAIS MORALES/00_conacyt/encodat-16')
pth <- './datos/datos-originales/'

df_hog <- read.csv( paste0(pth, 'ENCODAT_2016_2017_Hogar.csv') , sep= ';' )
df_ind <- readr::read_delim( paste0(pth, 'ENCODAT_2016_2017_Individual.csv'), delim = ';')
df_integ <- readr::read_delim( paste0(pth, 'ENCODAT_2016_2017_Integrantes.csv'), delim = ';')

# Individual

df_ind <- df_ind %>% mutate( id_hog = substring(id_pers, 1, 20 ) ) #%>% select(id_hog) 

df_ind <- df_ind %>% mutate( cve_mza = substring(id_pers, 1, 16 ),
                             cve_viv = substring(id_pers, 17, 20 ),
                             id_geo = paste0(cve_mza, '-', cve_viv)) #%>% select(id_hog) 

#
summary(df_ind$ds3)

# Solo hay personas de 12 a 65 años

df_ind <- df_ind %>% mutate ( edad_gpo = ifelse (ds3 < 18, "12-17", '18-65' ))

df_ind |> group_by(edad_gpo) |> summarise( total = n())

# 56, 867 encuestas
  # 12-17    12436
  # 18-65    44441
# 45,686 hogares en encuesta ya finalizada

### hogares 

df_cont_hog <- df_hog |> group_by(id_hogar) |> summarise(tot = n())

# 55,907 en la tabla de hogares

# Hay menos id de hogares ya al final 

df_ind_hog <- df_ind %>% group_by( id_hog) %>% summarise( ind_hog = n())

## se genera una tabla que contiene los hogares con cuest hogar y se le pegan los efectivos.

df_h_tot <- merge(df_cont_hog, df_ind_hog, by.x ='id_hogar', by.y = 'id_hog' ,  all.x = T)

df_h_tot$ind_hog <- df_h_tot$ind_hog %>% replace_na(99)

# Cuántos cuestionarios hay por hogar

df_h_tot %>% group_by( ind_hog) %>% summarise(t = n())


## Integrantes

df_integ <- df_integ %>% mutate(
  edad_gpo = ifelse ( h304 %in% 12:17 , '12-17', ifelse( h304 %in% 18:65, '18-65', 'Fuera de rango') ),
  edad_aplica = ifelse ( h304 %in% 12:65 , 'Si', 'Fuera de rango') ) 

df_integ %>% filter( id_hogar =='01001000121360020311') %>% select(edad_gpo)

# No están 10,221

# 34495 # una entrevista
# 11191 # Dos entrevistas

# 34495 + 11191 + 10221
# 55907

# 34495 + (2*11191)
# 56877 que son todas las entrevistas que se realizaron

### 


# Filtro 1 

# 1. De las que no se hicieron, había personas seleccionables?

df_nr <-  df_h_tot %>% filter( ind_hog == 99) %>% select(id_hogar)
df_nr <- df_nr$id_hogar

## FIltrado de los hogares en los que no se hizo ninguna encuesta

df_a <- df_integ %>% filter( id_hogar %in% df_nr)

df_a %>% group_by(id_hogar) %>% summarise(n()) # Es correcto el filtrado

df_a <- df_a %>% mutate(cve_edo = substring(id_hogar, 1, 2 ),
                        cve_mpio = substring(id_hogar, 3, 5 ))

## HOgar

df_hog <- df_hog %>% mutate(cve_edo = substring(id_hogar, 1, 2 ),cve_mpio = substring(id_hogar, 3, 5 ))


## Por entidad

df_edosr <- df_hog |> group_by(cve_edo) %>% summarise(total = n())

df_edonr <- df_a %>% group_by(id_hogar) %>% summarise(n()) # Es correcto el filtrado

df_edonr <- df_edonr %>% mutate(cve_edo = substring(id_hogar, 1, 2 ),
                cve_mpio = substring(id_hogar, 3, 5 ))

df_edonr <- df_edonr %>%  group_by(cve_edo) %>% summarise(total_no = n())

df_nr_edo <- merge(df_edosr,
                   df_edonr, by= 'cve_edo')

df_nr_edo <- df_nr_edo %>% mutate( difer = total - total_no,
                                   prop = difer/total*100,
                                   prop = round(prop,2)) 

write.csv(df_nr_edo, 'datos/datos-procesados/hogares_efectivos.csv')

###
# verificar procesamiento

df_5 <- df_a %>% group_by( id_hogar, edad_aplica) %>% summarise( tot = n())

df_5

plot(df_5)


#### ============ ####

df_eleg <-  df_a %>% 
  group_by( id_hogar, edad_aplica) %>% 
  summarise(t = n()) %>%
  ungroup() %>% 
  filter(edad_aplica == 'Si' ) %>%
  group_by(id_hogar) %>% summarise( tot = sum(t))

elegf <- df_eleg$id_hogar


df_6 <- df_integ %>% filter ( id_hogar %in% elegf) 

df_6 %>% group_by(id_hogar, edad_gpo) %>% summarise(cont = n()) %>% 
  pivot_wider( names_from = edad_gpo, values_from = cont) %>% clean_names() %>%
  View()

### No había alguien elegible

df_noeleg <- df_a %>% 
  group_by( id_hogar, edad_aplica) %>% 
  summarise(t = n()) %>%
  ungroup() %>% 
  filter(edad_aplica != 'Si' ) %>%
  group_by(id_hogar) %>% summarise( tot = sum(t))

noelegf <- df_noeleg$id_hogar

#

df_7 <- df_a %>% filter (! id_hogar %in% elegf) 

df_7 %>% group_by(id_hogar, edad_gpo) %>% summarise(cont = n()) %>% 
  pivot_wider( names_from = edad_gpo, values_from = cont) %>% clean_names() %>%
  View()


df_integ %>% filter(id_hogar == '01001000103220320121') %>% View()


## Falta encontrar los que en verdad no hubo persona elegible

df_b <- df_noeleg %>% filter ( ! id_hogar %in% elegf)

df_b <- df_b$id_hogar

df_noelegibles <- df_integ %>% filter(id_hogar %in% df_b)  %>% select(id_hogar, h304 )

summary(df_noelegibles$h304)

write.csv(df_noelegibles, 'datos/datos-procesados/hogares_no_eleg.csv')

# 5,426 había al menos una persona elegible

# 5426  / 55907 * 100 # En hogres la tasa en hogares fue de 88 %
# 

