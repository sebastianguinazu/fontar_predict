
## fit model: random forest -------------------------------------------------------------

source("scripts/functions.r")
source("scripts/libraries.r")
source("scripts/models_data.r")
source("scripts/models_train.r")

fontar_base = readRDS("working/fontar_base.rds")

models_id = 'lg'

file_models = glue('out/file_models_{models_id}.rds')
file_metrics = glue('out/file_metrics_{models_id}.csv')


## prepare the data ---------------------------------------------------------------------

ready_dats = get_xy(fontar_base %>% select(-id), impute_na = T)


# logistic  -----------------------------------------------------------------------------

dat = bind_cols(ready_dats$x_train, vd = ready_dats$y_train)
set.seed(88)
lg1 = glm(vd ~ ., family='binomial', data=dat)

# predict
prob_lg_train = pred_lg(lg1, ready_dats$x_train)
prob_lg_val = pred_lg(lg1, ready_dats$x_val)

# metrics
metrics(ready_dats$y_train, prob_lg_train)
metrics(ready_dats$y_val, prob_lg_val)

metric_gini(ready_dats$y_val, prob_lg_val)
