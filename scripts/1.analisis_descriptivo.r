# analisis descriptivo --------------------------------------------------------

source("scripts/functions.r")
source("scripts/libraries.r")


# bases crudas ----------------------------------------------------------------

# levanto la base original para tirar algunos estadisticos
path_benef = "raw/BDs_fontar.dta"
path_bd = "raw/w_fontar_predict.dta"

fontar_base_cruda = haven::read_dta(path_bd)
fontar_benef = haven::read_dta(path_benef)

#  tiro algunos estadisticos de base benef
unique(fontar_orig$cuit) %>% length()
table(fontar_benef$year)

# analizo la bd
targ_to_rm = c('target_i_com', 'target_i_org','target_i_proc', 'target_i_prod')
targ_to_rm_y = map(c("_1yearb", "_2yearb", "_3yearb", "__mean"), ~ targ_to_rm %+% .) %>%
  unlist()

fontar_base_cruda = fontar_base_cruda %>% 
  mutate(target_i_com = target_i_com_1yearb,
         target_i_org = target_i_org_1yearb,
         target_i_proc = target_i_proc_1yearb,
         target_i_prod = target_i_prod_1yearb) %>% 
  select(-one_of(targ_to_rm_y))

fontar_base_cruda = fontar_base_cruda %>% 
  mutate(vd = (target_i_prod == 1 | target_i_proc == 1 |
                 target_i_org == 1 | target_i_com == 1)) 

fontar_vd_table = rbind(table(fontar_base_cruda$target_i_prod) 
      ,table(fontar_base_cruda$target_i_proc)
      ,table(fontar_base_cruda$target_i_org)
      ,table(fontar_base_cruda$target_i_com)
      ,table(fontar_base_cruda$vd)
      )
row.names(fontar_vd_table) = c('Innovacion de producto', 'Innovacion de proceso',
                               'Innovacion organizacional', 'Innovacion comercial',
                               'Alguna innovacion')

# base final ------------------------------------------------------------------

fontar_base = readRDS("working/fontar_base.rds")

# vd
fontar_base


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

