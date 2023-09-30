pacman::p_load( readr, tidyr, dplyr, janitor, srvyr,
                kableExtra, stringr, foreign, utils)

setwd('C:/Users/ISAIS MORALES/00_conacyt/encodat-16')
pth <- './datos/datos-originales/'

df_hog <- read.csv( paste0(pth, 'ENCODAT_2016_2017_Hogar.csv') , sep= ';' )
df_ind <- readr::read_delim( paste0(pth, 'ENCODAT_2016_2017_Individual.csv'), delim = ';')
df_integ <- readr::read_delim( paste0(pth, 'ENCODAT_2016_2017_Integrantes.csv'), delim = ';')

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

###### Integrantes #####

df_inh1 <-  df_integ %>% group_by(id_hogar) %>% summarise( tothog = n())

nrow(df_inh1) # Total de hogares 55,907

# df_integ$intsel

df_aux <- df_integ %>% mutate(
    edad_gpo = ifelse (h304 %in% 12:17 , '12-17', ifelse( h304 %in% 18:65, '18-65', 'Fuera de rango') ) ) 


df_aux <-   df_aux %>% 
  group_by( id_hogar, edad_gpo) %>% summarise( t = n () ) %>% ungroup()


df_aux %>% 
    group_by( edad_gpo) %>% 
    summarise( n())

## 12-17 en hogar, totales

df_aux |> filter(edad_gpo == '12-17' )

h17 <- df_aux |> filter(edad_gpo == '12-17' ) |> 
  group_by(id_hogar) |> 
  summarise( hog17 = n()) 

h17 %>% arrange(-hog17)
summary(h17$hog17)

# 15,576

# 15,586

## 18-65 en hogar, totales

h65 <- df_aux |> filter(edad_gpo == '18-65' ) |> 
  group_by(id_hogar) |> 
  summarise( hog65 = n()) 

summary(h65$hog65)

# 12-17 y 18-65 en hogar, intersección

df_17y65 <- merge(h17, h65, by = 'id_hogar' )


### Hay adulto pero no adolesc

df_17y65 <- merge(h17, h65, by = 'id_hogar' )
fitl <- h17$id_hogar
h65 %>% filter( !id_hogar %in% fitl)


### Hay adolesc pero no agulto 

fitl <- h65$id_hogar
h17 %>% filter( !id_hogar %in% fitl)



### Hay adolesc pero no agulto 

fitl <- c(h65$id_hogar, h17$id_hogar)
fitl <- unique(fitl)

length(fitl)

df_hog %>% filter( !id_hogar %in% fitl) %>% group_by(id_hogar) %>% summarise( n())

# ==== Individual =========#

## Obtener el de de hogar 

df_ind <- df_ind %>% mutate( id_hog = substring(id_pers, 1, 20 ) ) #%>% select(id_hog) 

df_ind <- df_ind %>% mutate( cve_mza = substring(id_pers, 1, 16 ),
                             cve_viv = substring(id_pers, 17, 20 ),
                             id_geo = paste0(cve_mza, '-', cve_viv)) #%>% select(id_hog) 

df_ind %>% group_by( id_geo) %>% summarise( tot = n()) 

# Hay una disminunci+on de hogares

sum(table(unique(df_ind$id_pers)))

# 56, 877 personas encuestadas

sum(table(unique(df_ind$id_hog)))

# 45,686 hogares

# Máximo debemos tener a dos personas por hogar

df_ind %>% group_by(id_hog) %>% summarise( tot = n() ) %>% arrange( -tot)

# Sí se entrevistaron como máximo a 2 personas por hogar

## Unir los elementos del diseñoo muestral

df_m <- select(df_hog, id_hogar,   est_var,  ponde_hh,  code_upm,  estrato)

df_ind1 <- merge(df_ind, df_m, by.x = 'id_hog'  , by.y = 'id_hogar')
dim(df_ind1)

# Probamos que la edad es la misma

# Caso de una sola persona en hogar
df_m %>% 
  filter( id_hog == '01001000103220050011') %>% 
  select(id_hog, id_pers, ds3, ponde_ss, ponde_hh) 

# Caso de una sola persona en hogar
df_m %>% 
  filter( id_hog == '01001000103220120121') %>% 
  select(id_hog, id_pers, ds3, ponde_ss, ponde_hh) 

######

df_ind <- df_ind %>% mutate ( edad_gpo = ifelse (ds3 < 18, "12-17", '18-65' ))

table(df_ind$edad_gpo,df_ind$edad_gpo)

df_auz <- df_ind %>% group_by(id_hog ) %>% summarise ( cont = n())

df_auz2 <- df_auz %>% filter(cont >1)

df_valida <- df_ind %>% filter( id_hog %in% df_auz2$id_hog) %>% 
  select (id_hog, edad_gpo ,ds3 )   %>%
  arrange(id_hog )

df_valida %>% filter( id_hog == '04004000100120250011')

View(df_valida)

##

# Hay casos en los que se entrevistó a dos personas

# ----  #

df_m %>% 
  filter( id_hog == '04004000100120250011') %>% 
  select(id_hog, id_pers, ds3, ponde_ss, ponde_hh) 

###############################################################################

df_hog %>% select (id_hogar, locali)


df_ind <- df_ind %>% mutate( cve_mza = substring(id_pers, 1, 16 ),
                             cve_viv = substring(id_pers, 17, 20 ),
                             id_geo = paste0(cve_mza, '-', cve_viv)) #%>% select(id_hog) 

df_ind$cve_mza
