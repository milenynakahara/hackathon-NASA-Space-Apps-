library(tidyverse)
library(zoo)

source("funcoes.R", encoding = "UTF-8")


# Carregando os dados sobre o COVID
dados_covid <- readRDS("dados/dados_covid_mundo.rds")


# Casos novos
penultimo_dia <- as.Date("2020-05-29")
ultimo_dia <- as.Date("2020-05-30")

dados_novos_casos <- dados_covid %>% 
  filter(date %in% c(penultimo_dia, ultimo_dia)) %>% 
  group_by(Province.State, Country.Region, Lat,  Long) %>% 
  summarise(
    casos_confirmados = diff(casos_confirmados, na.rm = T),
    mortes = diff(mortes, na.rm = T),
    casos_curados = diff(casos_curados, na.rm = T)
  ) %>% 
  rename(lat = Lat, long = Long)


# Casos acumulados
dados_casos_acumulados <- dados_covid %>% 
  filter(date == ultimo_dia)



# Carregando os dados - para fazer o depara - países e continentes
df_paises_continentes <- read.csv2("dados/datasets_14947_19943_countryContinent.csv", sep = ",") %>% 
  dplyr::select(country, continent, sub_region)  %>% 
  as.data.frame() %>% 
  mutate(country = atualizar_nome_paises(as.character(country)))


# Juntando os dados dos países com os continentes - pelo nome do país
df_final_casos_novos <- dados_novos_casos %>% 
  left_join(df_paises_continentes, by=c("Country.Region"="country"))






