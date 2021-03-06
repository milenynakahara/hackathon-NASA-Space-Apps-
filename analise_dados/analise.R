library(tidyverse)
library(zoo)
library(openxlsx)
library(highcharter)
library(shiny)
library(leaflet)

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
  filter(date == ultimo_dia) %>% 
  rename(lat = Lat, long = Long)



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
  group_by(Country.Region, lat, long) %>% 
  summarise(
    casos_confirmados = sum(casos_confirmados, na.rm = T),
    mortes = sum(mortes, na.rm = T),
    casos_curados = sum(casos_curados, na.rm = T),
    pc_mortes_casos = (mortes/casos_confirmados)*100
  )


dados_periodo <- 
  dados_covid %>% 
  group_by(date) %>% 
  summarise(
    casos_confirmados = sum(casos_confirmados, na.rm = T),
    mortes = sum(mortes, na.rm = T),
    casos_curados = sum(casos_curados, na.rm = T)
  ) %>% 
  mutate(
    casos_confirmados1 = c(0, head(casos_confirmados, -1)),
    mortes1 = c(0, head(mortes, -1)),
    casos_curados1 = c(0, head(casos_curados, -1))
  )


# Casos acumulados ao longo do tempo ----



highchart() %>%
  hc_xAxis(categories = as.character(dados_periodo$date)) %>%
  hc_add_series(
    dados_periodo,
    name = "Casos",
    type = "line",
    hcaes(x = as.character(date), y = casos_confirmados)
  ) %>%
  hc_add_series(
    dados_periodo,
    name = "Mortes",
    type = "line",
    hcaes(x = as.character(date), y = mortes)
  ) %>%
  hc_add_series(
    dados_periodo,
    name = "Recuperados",
    type = "line",
    hcaes(x = as.character(date), y = casos_curados)
  ) %>%
  hc_tooltip(pointFormat = paste0("{point.series.name}: <strong>{point.y}</strong>")) %>%
  hc_colors(c("#ff9000", "#d60404", "#198c00")) %>% 
  hc_title(text = "COVID ao longo do tempo  - Dados acumulados")




# Casos novos ao longo do tempo ----

dados_novos_periodo <- 
  dados_periodo %>% 
  mutate(
    casos_confirmados = casos_confirmados - casos_confirmados1,
    mortes = mortes - mortes1,
    casos_curados = casos_curados - casos_curados1
  )



highchart() %>%
  hc_xAxis(categories = as.character(dados_novos_periodo$date)) %>%
  hc_add_series(
    dados_novos_periodo,
    name = "Casos",
    type = "line",
    hcaes(x = as.character(date), y = casos_confirmados)
  ) %>%
  hc_add_series(
    dados_novos_periodo,
    name = "Mortes",
    type = "line",
    hcaes(x = as.character(date), y = mortes)
  ) %>%
  hc_add_series(
    dados_novos_periodo,
    name = "Recuperados",
    type = "line",
    hcaes(x = as.character(date), y = casos_curados)
  ) %>%
  hc_tooltip(pointFormat = paste0("{point.series.name}: <strong>{point.y}</strong>")) %>%
  hc_colors(c("#ff9000", "#d60404", "#198c00")) %>% 
  hc_title(text = "COVID ao longo do tempo  - Dados novos")


# --------------------------------------------------------








########################################################
# Lendo os dados do IDH
dados_idh <- read.xlsx("dados/idh_2017.xlsx")

dados_idh_paises <- dados_idh %>% 
  filter(!is.na(HDI.rank)) %>% 
  mutate(posicao = HDI.rank) %>% 
  mutate(Country = as.character(atualizar_nome_paises2(as.character(Country)))) %>% 
  dplyr::select(-X5, -X7, -X9, -X11)



# Juntando os países com o valores acumulados ao IDH

df_acumulado_paises <- df_acumulado_paises %>% 
  left_join(dados_idh_paises, by = c("Country.Region"="Country"))


# Maiores número de casos - Top 20 países --------------------------------------
df_maiores_numeros_casos <- df_acumulado_paises %>% 
  arrange(desc(casos_confirmados)) %>% 
  head(20)

paises_20_num_casos <- df_maiores_numeros_casos %>% 
  pull(Country.Region) %>% 
  as.character()


# Top 50 maiores e menores IDH's
top_50_maiores_idhs <- dados_idh_paises %>% 
  arrange(HDI.rank) %>% 
  head(50) %>% 
  mutate(
    cores = ifelse(Country %in% paises_20_num_casos, "#990000", "#028975")
  )


# Top 50 maiores e menores IDH's
top_50_menores_idhs <- dados_idh_paises %>% 
  arrange(desc(HDI.rank)) %>% 
  head(50) %>% 
  mutate(
    cores = ifelse(Country %in% paises_20_num_casos, "#990000", "#028975")
  )

plot1 <- 
  highchart() %>%
  hc_title(
    text = "50 países com maior IDH em 2017"
  ) %>% 
  hc_xAxis(categories = as.character(top_50_maiores_idhs$`Country`)) %>% 
  hc_add_series(top_50_maiores_idhs, type = "bar", name = "50 países com maior IDH", hcaes(x = `Country`, y = `Human.Development.Index.(HDI).2017`, color = cores), color = "#028975") %>% 
  hc_tooltip(pointFormat = paste0('IDH: <strong>{point.y:.2f}%</strong><br>',
                                  'Posição no ranking (IDH): <strong>{point.posicao}</strong><br>'))

plot1$height <- 600


plot2 <- 
  highchart() %>%
  hc_title(
    text = "50 países com menor IDH em 2017"
  ) %>% 
  hc_xAxis(categories = as.character(top_50_menores_idhs$`Country`)) %>% 
  hc_add_series(top_50_menores_idhs, type = "bar", name = "50 países com menor IDH", hcaes(x = `Country`, y = `Human.Development.Index.(HDI).2017`, color = cores), color = "#028975") %>% 
  hc_tooltip(pointFormat = paste0('IDH: <strong>{point.y:.2f}%</strong><br>',
                                  'Posição no ranking (IDH): <strong>{point.posicao}</strong><br>'))

plot2$height <- 600

fluidRow(
  column(
    width = 6,
    plot1
  ),
  column(
    width = 6,
    plot2
  )
)


mapa_casos_confirmados <- df_acumulado_paises %>% 
  arrange(desc(casos_confirmados))

# Distribuição países - Casos confirmados
cor_confirmados <- "#ff9000"

texto_confirmados <- paste0(
  "<b>País ou região:</b> ", mapa_casos_confirmados$Country.Region, "<br/>",
  "<b>Província ou estado:</b> ", ifelse(mapa_casos_confirmados$Province.State == "", "Sem informação", as.character(mapa_casos_confirmados$Province.State)), "<br/>", 
  "<b>Total de casos confirmados:</b> ", mapa_casos_confirmados$casos_confirmados, "<br/>"
) %>%
  lapply(htmltools::HTML)

plot_confirmados <- 
  leaflet(mapa_casos_confirmados) %>% 
  addTiles()  %>% 
  addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(~long, ~lat, 
                   fillColor = cor_confirmados,
                   fillOpacity = 0.7,
                   color="white",
                   radius=c(15:1), stroke=FALSE,
                   label = texto_confirmados,
                   labelOptions = 
                     labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"),
                                   textsize = "13px", direction = "auto"))



df_mortes <- df_acumulado_paises %>%  
  arrange(desc(mortes))


cor_mortes <- "#d60404"

texto_mortes <- paste0(
  "<b>País ou região:</b> ", df_mortes$Country.Region, "<br/>",
  "<b>Província ou estado:</b> ", ifelse(df_mortes$Province.State == "", "Sem informação", as.character(df_mortes$Province.State)), "<br/>", 
  "<b>Total de mortes:</b> ", df_mortes$mortes, "<br/>"
) %>%
  lapply(htmltools::HTML)


plot_mortes <- 
  leaflet(df_mortes) %>% 
  addTiles()  %>% 
  addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(~long, ~lat, 
                   fillColor = cor_mortes,
                   fillOpacity = 0.7,
                   color="white",
                   radius=c(15:1), stroke=FALSE,
                   label = texto_mortes,
                   labelOptions = 
                     labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"),
                                   textsize = "13px", direction = "auto"))



fluidRow(
  column(
    width = 6,
    h3("Casos confirmados"),
    plot_confirmados
  ),
  column(
    width = 6,
    h3("óbitos"),
    plot_mortes
  )
)

# Gráfico de colunas 

highchart() %>%
  hc_title(
    text = "Top 20 países com maior número de casos"
  ) %>% 
  hc_xAxis(categories = as.character(df_maiores_numeros_casos$`Country.Region`)) %>% 
  hc_add_series(df_maiores_numeros_casos, type = "column", name = "Casos confirmados", hcaes(x = `Country.Region`, y = casos_confirmados), color = "#f7f4a5") %>% 
  hc_add_series(df_maiores_numeros_casos, type = "column", name = "óbitos", hcaes(x = `Country.Region`, y = mortes), color = "#a53b42") %>% 
  hc_add_series(df_maiores_numeros_casos, type = "column", name = "Casos curados", hcaes(x = `Country.Region`, y = casos_curados), color = "#3b69a5")



# Porcentagem de mortes em relação ao número de casos ---------------------------------

pct_maior_numero_casos <- 
  df_maiores_numeros_casos %>% 
  mutate(posicao = HDI.rank) %>%
  arrange(desc(pc_mortes_casos)) %>% 
  ungroup() %>% 
  head(20)

highchart() %>%
  hc_title(
    text = "% mortes dos país com maior número de casos"
  ) %>% 
  hc_xAxis(categories = as.character(pct_maior_numero_casos$`Country.Region`)) %>% 
  hc_add_series(pct_maior_numero_casos, type = "column", name = "% Mortes em relação aos casos", hcaes(x = `Country.Region`, y = pc_mortes_casos), color = "#960041") %>% 
  hc_tooltip(pointFormat = paste0('% Mortes em relação aos casos: <strong>{point.y:.2f}%</strong><br>',
                                  'Posição no ranking (IDH): <strong>{point.posicao}</strong><br>',
                                  'Número de casos: <strong>{point.casos_confirmados}</strong><br>',
                                  'Número de óbitos: <strong>{point.mortes}</strong><br>'))




# IDH desses - top 20 países
idh_top_20_paises_casos <- df_maiores_numeros_casos %>% 
  mutate(posicao = HDI.rank) %>% 
  arrange(`HDI.rank`) %>% 
  ungroup()

highchart() %>%
  hc_title(
    text = "IDH dos 20 países com maiores casos"
  ) %>% 
  hc_xAxis(categories = as.character(idh_top_20_paises_casos$`Country.Region`)) %>% 
  hc_add_series(idh_top_20_paises_casos, type = "bar", name = "IDH", hcaes(x = `Country.Region`, y = `Human.Development.Index.(HDI).2017`), color = "#028975") %>% 
  hc_tooltip(pointFormat = paste0('IDH: <strong>{point.y:.2f}%</strong><br>',
                                  'Posição no ranking (IDH): <strong>{point.posicao}</strong><br>'))


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------


# Verificando alguma relação - distribuição das variáveis - IDH - Casos
df_acumulado_paises %>%
  group_by(Country.Region) %>% 
  mutate(
    pct_casos = (casos_confirmados/sum(casos_confirmados))*100
  ) %>% 
  hchart("scatter",
         hcaes(
           y = pct_casos,
           x = `Human.Development.Index.(HDI).2017`,
           group = `Country.Region`
         )
  ) %>% 
  hc_tooltip(pointFormat = paste0('% casos confirmados: <strong>{point.y:.2f}</strong><br>',
                                  'IDH: <strong>{point.x:.2f}</strong><br>')) %>%
  hc_xAxis(title = list(text = "IDH")) %>% 
  hc_yAxis(title = list(text = "% casos confirmados")) %>% 
  hc_legend(enabled = T) %>% 
  hc_title(text = "Distribuição da % casos e o IDH dos países")


# Escolaridade média em anos  - Cassos
df_acumulado_paises %>% 
  group_by(Country.Region) %>% 
  mutate(
    pct_casos = (casos_confirmados/sum(casos_confirmados))*100
  ) %>% 
  hchart("scatter",
         hcaes(
           y = pct_casos,
           x = `Mean.years.of.schooling.2017`,
           group = `Country.Region`
         )
  ) %>% 
  hc_tooltip(pointFormat = paste0('% de casos confirmados: <strong>{point.y:.2f}</strong><br>',
                                  'Escolaridade média em anos: <strong>{point.x:.2f}</strong><br>')) %>%
  hc_xAxis(title = list(text = "Escolaridade média em anos")) %>% 
  hc_yAxis(title = list(text = "% de casos confirmados")) %>% 
  hc_legend(enabled = T) %>% 
  hc_title(text = "Distribuição da % casos e a escolaridade média em anos de cada país")




# Verificando alguma relação - distribuição das variáveis - IDH - Mortes
df_acumulado_paises %>%
  group_by(Country.Region) %>% 
  mutate(
    pct_mortes = (mortes/sum(mortes))*100
  ) %>%  
  hchart("scatter",
         hcaes(
           y = pct_mortes,
           x = `Human.Development.Index.(HDI).2017`,
           group = `Country.Region`
         )
  ) %>% 
  hc_tooltip(pointFormat = paste0('% de Óbitos: <strong>{point.y:.2f}%</strong><br>',
                                  'IDH: <strong>{point.x:.2f}</strong><br>')) %>%
  hc_xAxis(title = list(text = "IDH")) %>% 
  hc_yAxis(title = list(text = "% óbitos")) %>% 
  hc_legend(enabled = T) %>% 
  hc_title(text = "Distribuição da % de óbitos e o IDH dos países")


# Escolaridade média em anos  - Mortes
df_acumulado_paises %>% 
  group_by(Country.Region) %>% 
  mutate(
    pct_mortes = (mortes/sum(mortes))*100
  ) %>% 
  hchart("scatter",
         hcaes(
           y = pct_mortes,
           x = `Mean.years.of.schooling.2017`,
           group = `Country.Region`
         )
  ) %>% 
  hc_tooltip(pointFormat = paste0('% de óbitos: <strong>{point.y:.2f}%</strong><br>',
                                  'Escolaridade média em anos: <strong>{point.x:.2f}</strong><br>')) %>%
  hc_xAxis(title = list(text = "Escolaridade média em anos")) %>% 
  hc_yAxis(title = list(text = "% de óbitos")) %>% 
  hc_legend(enabled = T) %>% 
  hc_title(text = "Distribuição da % de óbitos e a escolaridade média em anos dos países")




# ------------------------------------------------------------------------------
# Carregando dados da densidade populacional

df_densidade_populacional <- read.csv2("dados/datasets_507962_1091873_population_by_country_2020.csv", sep=",") %>% 
  mutate(
    `Country.Region` = atualizar_nome_paises2(Country..or.dependency.),
    `% World.Share` = removendo_porcentagem(World.Share),
    `% Urban.Pop..` = removendo_porcentagem(Urban.Pop..),
    `% Yearly.Change` = removendo_porcentagem(Yearly.Change)
  ) %>% 
  dplyr::select(-Yearly.Change, -`Urban.Pop..`, -`World.Share`)


# Juntando os dados dos países com maior número de casos com os dados da densidade populacional

df_populacao <- 
  df_acumulado_paises %>% 
  left_join(df_densidade_populacional, by="Country.Region") %>% 
  group_by(Country.Region) %>% 
  summarise(
    `%casos_populacao` = sum(casos_confirmados, na.rm = T)/sum(`Population..2020.`, na.rm = T),
    populacao = sum(`Population..2020.`, na.rm = T),
    `% Urban.Pop..` = unique(`% Urban.Pop..`),
    `densidade_pop_km2` = unique(`Density..P.KmÂ².`)
  ) %>% 
  ungroup()


df_populacao %>% 
  hchart("scatter",
         hcaes(
           y = `%casos_populacao`,
           x = densidade_pop_km2,
           group = `Country.Region`
         )
  ) %>% 
  hc_tooltip(pointFormat = paste0('% de casos na população: <strong>{point.y:.2f}%</strong><br>',
                                  'Densidade por Km²: <strong>{point.x:.2f}</strong><br>')) %>%
  hc_xAxis(title = list(text = "Densidade por km²")) %>% 
  hc_yAxis(title = list(text = "% casos na população")) %>% 
  hc_legend(enabled = T) %>% 
  hc_title(text = "Distribuição da % casos na população e a densidade por km²")
