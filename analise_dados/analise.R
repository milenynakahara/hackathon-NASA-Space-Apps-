library(tidyverse)
library(zoo)
library(openxlsx)
library(highcharter)


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
  mutate(
    country = atualizar_nome_paises(as.character(country)),
    continente_sub = paste(continent, sub_region, sep = " - ")
  )


# Juntando os dados dos países com os continentes - pelo nome do país
df_final_casos_novos <- dados_novos_casos %>% 
  left_join(df_paises_continentes, by=c("Country.Region"="country"))


df_final_casos_acumulados <- dados_casos_acumulados %>% 
  left_join(df_paises_continentes, by=c("Country.Region"="country"))



# Visualizando os ultimo status do covid nos países


df_acumulado_paises <- df_final_casos_acumulados %>% 
  group_by(Country.Region) %>% 
  summarise(
    casos_confirmados = sum(casos_confirmados, na.rm = T),
    mortes = sum(mortes, na.rm = T),
    casos_curados = sum(casos_curados, na.rm = T)
  )

########################################################
# Lendo os dados do IDH
dados_idh <- read.xlsx("dados/idh_2017.xlsx")

dados_idh_paises <- dados_idh %>% 
  filter(!is.na(HDI.rank)) %>% 
  dplyr::select(-X5, -X7, -X9, -X11)



# Juntando os países com o valores acumulados ao IDH

df_acumulado_paises <- df_acumulado_paises %>% 
  left_join(dados_idh_paises, by = c("Country.Region"="Country"))


# Top 20 maiores IDH's
top_20_maiores_idhs <- df_acumulado_paises %>% 
  filter(HDI.rank %in% 1:20) %>% 
  arrange(HDI.rank)


# Top 20 menores IDH's
top_20_menores_idhs <- df_acumulado_paises %>% 
  arrange(desc(HDI.rank)) %>% 
  head(20)




########################################################


# Visualizando o IDH dos países em 2017
hchart(
  dados_idh_paises,
  "column",
  hcaes(
    x = as.character(Country), 
    y = `Human.Development.Index.(HDI).2017`
  )
) %>% 
  hc_plotOptions(
    column = list(
      dataLabels = list(
        enabled = TRUE, 
        format = '{y:.,2f}'
      )
    )
  ) %>%
  hc_yAxis(title = list(text = "IDH 2017")) %>% 
  hc_xAxis(title = list(text = "Países")) %>% 
  hc_title(text = "IDH por países em 2017") 
  # hc_colors(as.character(unique(dados$color)))
  

