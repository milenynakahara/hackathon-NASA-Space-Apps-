library(tidyverse)
library(zoo)

source("funcoes.R", encoding = "UTF-8")


# Carregando os dados
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


