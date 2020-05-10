## analisis descriptivo ---------------------------------------------------------

# exploro la distribucion de las vars

View(explora$numeric)

var_miss = explora$numeric %>% 
  filter(missing > nrow(fontar_base) * 0.95) %>% select(variable)


class(fontar_base$clae2)
typeof(fontar_base$clae2)

length(unique(fontar_base$clae2)) # 62
length(unique(fontar_base$clae3)) # 121

class(fontar_base$link)
length(unique(fontar_base$link)) # 116

table(fontar_base$year_contrato)
sum(is.na(fontar_base$year_contrato)) # 113

table(fontar_base$year_inscripcion)
sum(is.na(fontar_base$year_inscripcion)) # 113

fontar_base$ventas_1yearb

is.infinite(fontar_base$empleo__trend)

