corrigir_data <- function(dados) {
  
  dados <- dados %>% 
    mutate(
      date = gsub(".20", "/2020", date),
      date = gsub("\\.", "/", date),
      date = gsub("X", "0", date),
      date = gsub("/", "-", date),
      date = as.Date(date, "%m-%d-%Y")
    )
  
  return(dados)
  
}


gerandos_dados_mundo <- function() {
  
  link_confirmados <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
  link_obitos <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
  link_curados <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"
  
  df_casos_confirmados <- 
    read.csv(link_confirmados) %>% 
    pivot_longer(
      cols = contains(".20"),
      names_to = "date",
      values_to = "casos_confirmados",
      values_drop_na = TRUE
    ) %>% 
    corrigir_data()
  
  df_mortes <- 
    read.csv(link_obitos) %>% 
    pivot_longer(
      cols = contains(".20"),
      names_to = "date",
      values_to = "mortes",
      values_drop_na = TRUE
    ) %>% 
    corrigir_data()
  
  df_casos_curados <- 
    read.csv(link_curados) %>% 
    pivot_longer(
      cols = contains(".20"),
      names_to = "date",
      values_to = "casos_curados",
      values_drop_na = TRUE
    ) %>% 
    corrigir_data()
  
  dados <- left_join(
    df_casos_confirmados, 
    df_mortes,
    by = c("Province.State", "Country.Region", "Lat", "Long", "date")
  ) %>% 
    left_join(
      df_casos_curados,
      by = c("Province.State", "Country.Region", "Lat", "Long", "date")
    ) %>% 
    filter(!is.na(date)) %>% 
    mutate(
      Mes = as.yearmon(date)
    )
  
  saveRDS(dados, "dados/dados_covid_mundo.rds", version = 2)
  
}




atualizar_nome_paises <- function(text) {
  
  text <- 
    text %>% 
    str_trim() %>% 
    str_replace_all("Bolivia", "Bolivia") %>% 
    str_replace_all("Brunei", "Brunei") %>% 
    str_replace_all("Burma", "Burma") %>% 
    str_replace_all("Brazzaville", "Congo (Brazzaville)") %>% 
    str_replace_all("Kinshasa", "Congo (Kinshasa)") %>% 
    str_replace_all("d'Ivoire", "Cote d'Ivoire") %>% 
    str_replace_all("Czechia", "Czechia") %>% 
    str_replace_all("Korea", "Korea, South") %>% 
    str_replace_all("Diamond Princess", "Diamond Princess") %>% 
    str_replace_all("Eswatini", "Eswatini") %>% 
    str_replace_all("Iran (Islamic Republic of)", "Iran") %>% 
    str_replace_all("Kosovo", "Kosovo") %>% 
    str_replace_all("Laos", "Laos") %>% 
    str_replace_all("Moldova", "Moldova") %>% 
    str_replace_all("Zaandam", "MS Zaandam") %>% 
    str_replace_all("Macedonia", "North Macedonia") %>% 
    str_replace_all("Russian Federation", "Russia") %>% 
    str_replace_all("Syria", "Syria") %>% 
    str_replace_all("Taiwan", "Taiwan*") %>% 
    str_replace_all("Tanzania", "Tanzania") %>% 
    str_replace_all("United Kingdom", "United Kingdom") %>% 
    str_replace_all("United States of America", "US") %>% 
    str_replace_all("Venezuela", "Venezuela") %>% 
    str_replace_all("Vietnam", "Vietnam")
  
  return(text)
  
}



atualizar_nome_paises2 <- function(text) {
  
  text <- 
    text %>% 
    str_trim() %>% 
    str_replace_all("Bolivia", "Bolivia") %>% 
    str_replace_all("Brunei", "Brunei") %>% 
    str_replace_all("Burma", "Burma") %>% 
    str_replace_all("Brazzaville", "Congo (Brazzaville)") %>% 
    str_replace_all("Kinshasa", "Congo (Kinshasa)") %>% 
    str_replace_all("d'Ivoire", "Cote d'Ivoire") %>% 
    str_replace_all("Czechia", "Czechia") %>% 
    str_replace_all("Korea", "Korea, South") %>% 
    str_replace_all("Diamond Princess", "Diamond Princess") %>% 
    str_replace_all("Eswatini", "Eswatini") %>% 
    str_replace_all("\\(Islamic Republic of\\)", "") %>% 
    str_replace_all("Kosovo", "Kosovo") %>% 
    str_replace_all("Laos", "Laos") %>% 
    str_replace_all("Moldova", "Moldova") %>% 
    str_replace_all("Zaandam", "MS Zaandam") %>% 
    str_replace_all("Macedonia", "North Macedonia") %>% 
    str_replace_all("Russian Federation", "Russia") %>% 
    str_replace_all("Syria", "Syria") %>% 
    str_replace_all("Taiwan", "Taiwan*") %>% 
    str_replace_all("Tanzania", "Tanzania") %>% 
    str_replace_all("United Kingdom", "United Kingdom") %>% 
    str_replace_all("United States", "US") %>% 
    str_replace_all("Venezuela", "Venezuela") %>% 
    str_replace_all("Vietnam", "Vietnam") %>% 
    str_trim()
  
  return(text)
  
}
