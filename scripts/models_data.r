# DATA PREP  ----------------------------------------------------------------------------

# receta transformaciones
receta = function(dat
                  , impute_na_num=F, minmax=F, create_vars=F, scale_nn=F
                  , vars_to_rm=NULL) {
  # remove optional vars
  if (!is.null(vars_to_rm)) {
    dat = dat %>% select(-one_of(vars_to_rm))
  }
  # start recipe
  rec = recipe(vd ~ ., data=dat) %>% 
    step_rm(vd)
  # impute NA numeric
  if (impute_na_num) {
    rec = rec %>% step_mutate_at(all_numeric(), -all_outcomes()
                                 , fn = function(x) ifelse(is.na(x), 0, x))
    rec = rec %>% step_mutate_at(all_numeric(), -all_outcomes()
                                 , fn = function(x) ifelse(is.infinite(x), 0, x))
  }
  # zero-variance filter
  rec = rec %>% 
    step_zv(all_predictors())
  # minmax
  if (minmax) {
    rec = rec %>% step_range(all_numeric(), -all_outcomes()
                             , min=0, max=1)
  }
  # scalling -1, 1 para NN
  if (scale_nn) {
    rec = rec %>% 
      step_center(all_numeric(), -all_outcomes()) %>%
      step_range(all_numeric(), -all_outcomes(), min=-1, max=1)
  }
  # transform vars
  if (create_vars) {
    rec = rec %>% 
      step_mutate_at(
        all_numeric(), -all_outcomes()
        ,fn = list(
          log = function(x) ifelse(is.na(log(x+1)) | is.infinite(log(x+1)), 0, log(x+1))
          ,sq = function(x) ifelse(x<0, x, x**2)
          ,sqrt = function(x) ifelse(x<0, x, x**(1/2))
          ,cbtr = function(x) ifelse(x<0, x, x**(1/3))
      ))
  }
  # treatment for categorical vars if any
  var_types = summary(rec)$type; var_roles = summary(rec)$role
  if (any((var_types == 'nominal') & (var_roles == 'predictor'))) {
    rec = rec %>% 
      # vacio es NA (y que no pase a factor aun)
      step_mutate_at(
        all_nominal(), -all_outcomes()
        , fn = function(x)
            as.character(ifelse(x %in% c('', ' '), NA_character_, as.character(x)))
      ) %>% 
      # strings as factors
      step_string2factor(all_nominal(), -all_outcomes()) %>% 
      # NA como 'unknown'
      step_unknown(all_nominal(), -all_outcomes(), new_level='unknown') %>% 
      # agrupo low freq en otros
      step_other(all_nominal(), -all_outcomes(), threshold=0.01, other='otros')
  }
  return(rec)
}

# prepare (x-y train-val)
get_xy = function(dat
                  , impute_na=F, minmax=F, create_vars=F, scale_nn=F) {
  # particion de la data
  set.seed(123)
  random = runif(1:nrow(dat)) > 0.8
  dat_train = dat[random < 0.8,]
  dat_val = dat[random >= 0.8,]
  # cocino la receta
  receta_trained = receta(
    dat_train, impute_na, minmax, create_vars, scale_nn
  ) %>% 
    prep(retain=T)
  out = list()
  # training
  out[['x_train']] = juice(receta_trained) %>% as.data.frame()
  out[['y_train']] = dat_train$vd
  # validation
  out[['x_val']] = bake(receta_trained, new_data=dat_val) %>% as.data.frame()
  out[['y_val']] = dat_val$vd  
  # receta entrenada
  out[['receta']] = receta_trained
  return(out)
} 