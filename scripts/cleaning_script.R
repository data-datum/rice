#script de limpieza

#ingreso datos 
rice<-read_excel("data/rice2.xlsx")
#nombres de columnas
library(janitor)
rice<-rice%>%
  clean_names()

#variables de la columna class
rice2 <- rice2  %>%
  mutate(Class = str_replace(Class, "10% Adulteration", "10_adulteration"))%>%
  mutate(Class = str_replace(Class, "20% Adulteration", "20_adulteration"))%>%
  mutate(Class = str_replace(Class, "30% Adulteration", "30_adulteration"))%>%
  mutate(Class = str_replace(Class, "40% Adulteration", "40_adulteration"))%>%
  mutate(Class = str_replace(Class, "Pure variety", "pure_variety"))

  
#la columna Class esta codificada como caracter y necesito que sea FACTOR
class(rice$Class)
rice2$class <-as.factor(rice2$class)
str(rice2)

