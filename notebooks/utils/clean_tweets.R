# Functions clean data
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# clean_tweets
#
# - Convierte la fecha en UTC para el filtrado por fechas
# - Corrige el id del tweet que twscaper lo da mal
# - Convierte a carácter el id del usuario 
# - Elimina tweets repetidos
# - Elimina tweets fuera del rango de fechas
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Limpiar, ordenar y quitar repetidos
clean_tweets <- function(df, since, until){
df <- df %>%
  mutate(
    date = force_tz(date, "UTC"), # Ponemos hora UTC 
    text = str_replace_all (text, '[\n\r]+',' '), # quitamos saltos de línea
    user_displayname = str_replace_all (user_displayname, '[\n\r]+',' '), 
    id = sub(".*status/", "", url), # corregimos el id, que no lo da bien
    user_id =  as.character(user_id) # convertimos el id user en carácter
  ) %>% 
  group_by(url)  %>%
    slice(1) %>%
  ungroup() %>%
  arrange(date) %>%
  filter(date >= since & date <= until)
  return (df)
}
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# clean_text
#
# - Elimina los saltos de línea para visualizar bien lo csv en editores de texto
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Limpiar saltos de linea
clean_text <- function(df, since, until){
  df <- df %>%
    mutate(
      text = str_replace_all (text, '[\n\r]+',' '), # quitamos saltos de línea
      user_displayname = str_replace_all (user_displayname, '[\n\r]+',' '), 
    ) 

  return (df)
}