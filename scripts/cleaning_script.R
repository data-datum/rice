#script de limpieza
#nombres de columnas
library(janitor)
rice<-rice%>%
  clean_names()

#variables de la columna class

rice <- rice  %>%
  mutate(class = str_replace(class, "10% Adulteration", "10_adulteration"))%>%
  mutate(class = str_replace(class, "20% Adulteration", "20_adulteration"))%>%
  mutate(class = str_replace(class, "30% Adulteration", "30_adulteration"))%>%
  mutate(class = str_replace(class, "40% Adulteration", "40_adulteration"))%>%
  mutate(class = str_replace(class, "Pure variety", "pure_variety"))

  
