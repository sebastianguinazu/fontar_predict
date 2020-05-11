
## fit model: gradient boosting machine -------------------------------------------------

source("scripts/functions.r")
source("scripts/libraries.r")
source("scripts/models_data.r")
source("scripts/models_train.r")
source("scripts/models_perf.r")

fontar_base = readRDS("working/fontar_base.rds")

models_id = 'gb'

file_models = glue('working/file_models_{models_id}.rds')

file_metrics = glue('working/file_metrics_{models_id}.csv')
export_metrics = glue('out/file_metrics_{models_id}.csv')


## prepare the data ---------------------------------------------------------------------

ready_dats = get_xy(fontar_base %>% select(-id), impute_na = T)


## hyperparameter search ----------------------------------------------------------------

# grid of hiperpar
set.seed(123)
training_grid = crossing(
  ntrees = seq(100, 500, by = 100)   
  ,learn_rate = seq(0.001, 0.01, by = 0.001)
  ,int_depth = seq(1, 5, by = 1)
) %>% sample_n(50)


## load existing models -----------------------------------------------------------------

if (file.exists(file_models)) {
  modelos = readRDS(file_models)
} else {
  modelos = tibble()
}


## train gbm ----------------------------------------------------------------------------

# train only for new grid values
temp = modelos %>% select(-starts_with('modelo')) %>% bind_rows(training_grid[0,])
new_modelos = dplyr::setdiff(training_grid, temp) %>% 
  mutate(
    modelo = pmap(
      list(ntrees, learn_rate, int_depth)
      ,function(n, l, i) train_gb(ready_dats$x_train, ready_dats$y_train
                                  ,ntrees=n, learn_rate=l, int_depth=i)
    )
  )

  # append to modelos viejos
modelos = bind_rows(modelos, new_modelos)

# save models
saveRDS(modelos, file_models)


## performance---------------------------------------------------------------------------

# predict and merics
metricas_train = modelos %>% 
  mutate(
    probs = map(modelo, function(m) pred_gb(m, ready_dats$x_train))
    ,metricas = map(probs, function(p) metrics(ready_dats$y_train, p))
  ) %>% 
  select(-modelo, -probs) %>% 
  unnest(cols=metricas) %>% 
  mutate(datos = 'training')
metricas_val = modelos %>% 
  mutate(
    probs = map(modelo, function(m) pred_gb(m, ready_dats$x_val))
    ,metricas = map(probs, function(p) metrics(ready_dats$y_val, p))
  ) %>% 
  select(-modelo, -probs) %>% 
  unnest(cols=metricas) %>% 
  mutate(datos = 'validation')
metricas = bind_rows(metricas_train, metricas_val) %>% 
  mutate(modelo = 'GB') %>% 
  select(modelo, datos, everything())

# reshape con hiperparametros
metricas_tmp = metricas %>% 
  mutate(hiperpar = paste('ntrees:', ntrees, 
                          '-learnr:', learn_rate, 
                          '-intdepth:',int_depth, sep = '')) %>%
  select(modelo, datos, hiperpar, ks, auc) 

metricas_w = metricas_tmp %>% 
  filter(datos == 'training') %>% select(-datos) %>% 
  inner_join(metricas_tmp %>% filter(datos == 'validation') %>% select(-datos),
             by = c('modelo', 'hiperpar'),
             suffix = c("_train", "_val")) %>% 
  mutate(dif_auc = auc_train - auc_val,
         dif_ks = ks_train - ks_val)

# save 
saveRDS(metricas_w, file_metrics)
write_csv(metricas_w, export_metrics)
