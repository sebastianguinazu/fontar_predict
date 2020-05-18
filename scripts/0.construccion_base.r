
## FONTAR PREDICT -----------------------------------------------------------------------

source("scripts/functions.r")
source("scripts/libraries.r")


## importo la base de datos -------------------------------------------------------------

path_bd = "raw/w_fontar_predict.dta"

fontar_base = haven::read_dta(path_bd)
fontar_base$id = as.character(fontar_base$id)
fontar_base$id %>% head()


## analizo algunas var y corrijo ---------------------------------------------------------

dim(fontar_base)
str(fontar_base)

# cantidad de empresas
length(unique(fontar_base$id)) # 989
nrow(fontar_base) # 989 -> una linea por empresa

# analisis de columnas
ncol(fontar_base) # -> 422
colnames(fontar_base)

# to character
to_char = c('clae2', 'clae3', 'clae4', 'clae6', 'clae6_act_sec1', 'clae6_act_sec2',
              'codprov', 'link', 'cpa')
fontar_base = fontar_base %>%  
  mutate_at(to_char, funs(as.character(.))) 


# variables para analizar
hist(fontar_base$ai_empleo_D1yearb)
fontar_base %>% filter(!is.na(ai_empleo_D1yearb)) %>% nrow() # 843
hist(fontar_base$ai_empleo1yearb)
hist(fontar_base$uf_id_1yearb) # empleados en unidad formal de I+D
hist(fontar_base$unf_id_1yearb) # empleados en unidad no formal de I+D

hist(fontar_base$ventas_D1yearb)

plot(fontar_base$ventas_D1yearb, fontar_base$VtasMI_1yearb)
plot(fontar_base$ventas_D1yearb, fontar_base$ventas_1yearb)
hist(fontar_base$ventas_D1yearb - fontar_base$VtasMI_1yearb)
hist(fontar_base$ventas_1yearb - fontar_base$VtasMI_1yearb)
hist(fontar_base$ventas_1yearb - fontar_base$VtasMI_1yearb - fontar_base$VtasMEx_1yearb - fontar_base$VtasRev_1yearb)
sum(is.na(fontar_base$ventas_D1yearb)) # 71
nrow(subset(fontar_base, !is.na(ventas_D1yearb))) # 918

# corrijo vars: id_ventas ; id_empleo ; ai_ventas
hist(fontar_base$id_ventas1yearb)
nrow(subset(fontar_base, !is.na(id_ventas1yearb))) # 692
describe(fontar_base$id_ventas1yearb)
summary(fontar_base$id_ventas1yearb)
# parece una propocion
fontar_base %>% filter(id_ventas1yearb>1) %>% select(id, id_ventas1yearb) %>% nrow()

# reemplazo los valores mayores a 1
vars_to_mod = c('id_ventas', 'id_empleo', 'ai_ventas')
fontar_base = fontar_base %>% 
  mod_vars(vars_to_mod)

hist(fontar_base$id_empleo1yearb)
nrow(subset(fontar_base, !is.na(id_empleo1yearb))) # 674
describe(fontar_base$id_empleo1yearb)
summary(fontar_base$id_empleo1yearb)

# antiguedad
hist(fontar_base$year_contrato)
table(fontar_base$year_contrato)
hist(fontar_base$year_inscripcion)
table(fontar_base$year_inscripcion)
table(fontar_base %>% filter(year_contrato == 1901) %>% select(year_inscripcion)) 

fontar_base = fontar_base %>% 
  mutate(anio_creacion = case_when(
    year_contrato == 1901 ~ year_inscripcion,
    !is.na(year_contrato) ~ year_contrato,
    TRUE ~ year_inscripcion)) %>%
  select (-one_of(c('year_inscripcion', 'year_contrato')))

## me quedo solo con las var que se que son  --------------------------------------------

# elimino las variables que no se que son
path_vars = glue(getwd(), "/raw/fontar_vars.xlsx")
vars_ok = readxl::read_excel(path_vars)

vars_to_rm = subset(vars_ok, ok == 0)$Variable

vars_to_rm = map(c("1yearb", "2yearb", "3yearb"), ~ vars_to_rm %+% .) %>% unlist()

# agrego otras vars a eliminar
vars_to_rm = c(vars_to_rm, 'd_fontar_yearmin')
fontar_base = fontar_base %>% select(-one_of(vars_to_rm))


## elimino variables que me parecen innecesarias ----------------------------------------

vars_innec = c(
  fontar_base %>% select(starts_with('afip')) %>% colnames(),
  fontar_base %>% select(starts_with('aux')) %>% colnames(),
  fontar_base %>% select(starts_with('kilos')) %>% colnames(),
  fontar_base %>% select(starts_with('empleo_m')) %>% colnames(),
  fontar_base %>% select(starts_with('sexo')) %>% colnames()
  )

fontar_base = fontar_base %>% select(-one_of(vars_innec))


## creo variables promedio de las que tienen tres anios ---------------------------------

varyears = colnames(fontar_base)[endsWith(colnames(fontar_base), "yearb")]
length(varyears) # 372
vars = unique(substr(varyears, 1, nchar(varyears)-nchar("xyearb")))

fontar_base[,endsWith(colnames(fontar_base), "yearb")]

# paso la base a long y genero atributos = mean de los tres anios
fontar_atr = fontar_base %>% 
    select(id, varyears) %>% 
    gather(key, value, -id) %>%
    mutate(atr = substr(key, 1, nchar(key)-6),
           time = substr(key, nchar(key)-5, nchar(key))) %>%
    select(-key) %>% 
    spread(atr, value) %>% 
    group_by(id) %>% 
    summarise_all(funs(mean))

colnames(fontar_atr) = 
    c(colnames(fontar_atr[1:2]), colnames(fontar_atr[3:length(colnames(fontar_atr))]) %+% "_mean")

# creo algunas var dummies con la info que arme
fontar_atr = fontar_atr %>% 
    mutate(d_uf_id = ifelse(unf_id__mean > 0, 1, 0),
           d_unf_id = ifelse(unf_id__mean > 0, 1, 0)) 

# junto la base
fontar_base = fontar_base %>% 
  left_join(fontar_atr, by = 'id')

# creo algunas var de tendencia
for_trend = c('ai_ventas', 'cant_pdest_expo', 'cant_posic_expo', 'credito',
              'empleo_', 'fob_expo', 'id_empleo', 
              'id_ventas', 'l_prof_', 'salario')

fontar_base = fontar_base %>% var_trend(for_trend)


## elimino las variables con muchos missing -----------------------------------

miss_lim = 0.96

explora = skimr::skim_to_list(fontar_base)

var_miss = explora$numeric %>% 
  filter(missing > nrow(fontar_base) * miss_lim) %>% select(variable) %>% 
  unlist()

fontar_base = fontar_base %>% select(-one_of(var_miss))


## vd -----------------------------------------------------------------------

targ_to_rm = c('target_i_com', 'target_i_org','target_i_proc', 'target_i_prod')
targ_to_rm_y = map(c("_1yearb", "_2yearb", "_3yearb", "__mean"), ~ targ_to_rm %+% .) %>%
  unlist()

fontar_base = fontar_base %>% 
  mutate(target_i_com = target_i_com_1yearb,
         target_i_org = target_i_org_1yearb,
         target_i_proc = target_i_proc_1yearb,
         target_i_prod = target_i_prod_1yearb) %>% 
  select(-one_of(targ_to_rm_y))

table(fontar_base$target_i_prod)
table(fontar_base$target_i_proc)
table(fontar_base$target_i_org)
table(fontar_base$target_i_com)

table(fontar_base$target_i_prod, fontar_base$target_i_org)
table(fontar_base$target_i_prod, fontar_base$target_i_proc)

# vd final como combinacion de las anteriores
fontar_base = fontar_base %>% 
  mutate(vd = (target_i_prod == 1 | target_i_proc == 1 |
                target_i_org == 1 | fontar_base$target_i_com)) 

table(fontar_base$vd)

# termino de sacar las variables target
fontar_base = fontar_base %>% 
  select(-one_of(targ_to_rm))


## guardo la base final ------------------------------------------------------

# View(fontar_base[1:100,])

saveRDS(fontar_base, "working/fontar_base.rds")

