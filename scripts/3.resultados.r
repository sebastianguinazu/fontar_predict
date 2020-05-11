
## FONTAR PREDICT - analisis de resultados ------------------------------------

# me quedo con el mejor modelo (por ahora manual)


models_id = 'rf'
file_models = glue('working/file_models_{models_id}.rds')
file_metrics = glue('working/file_metrics_{models_id}.csv')

modelos = readRDS(file_models)
metricas = readRDS(file_metrics)

metricas = metricas %>% arrange(desc(auc_val))

metricas_bm = metricas$hiperpar[[1]]
modelos_bm = modelos %>% 
  mutate(hiperpar = paste('ntrees:', ntrees, 
                          '-minobj:', minobj, 
                          '-varstry:',varstry, sep = '')) %>% 
  filter(hiperpar == metricas_bm)


# metricas de performance -----------------------------------------------------

# creo dataframe con datos de valicacion
df_val = data.frame(
  prob = pred_rf(modelos_bm[["modelo"]][[1]], ready_dats$x_val),
  vd = ready_dats$y_val)


# curva ROC
f_roc_curve = roc_curve(df_val, as.factor(vd), prob)

# AUC
f_roc_curve %>% ggplot(aes(x=1-specificity, y=sensitivity)) +
  geom_line() +
  geom_abline(linetype = "dashed") +
  scale_fill_grey() + theme_classic() +
  NULL

# ks
f_roc_curve = f_roc_curve %>% mutate(dif = sensitivity - (1-specificity))
f_ks = f_roc_curve[which.max(f_roc_curve$dif),]
f_roc_curve %>% ggplot() +
  geom_line(aes(x=.threshold, y=sensitivity)) +
  geom_line(aes(x=.threshold, y=1-specificity)) +
  geom_vline(xintercept=f_ks$.threshold) +
  scale_fill_grey() + theme_classic() +
  NULL

# matriz de confusion
f_mat_conf = df_val %>% 
  mutate(pred = prob > f_ks$.threshold)
table(f_mat_conf$vd, f_mat_conf$pred) %>% addmargins()
table(f_mat_conf$vd, f_mat_conf$pred) %>% prop.table()

# grafico de distribucion de prob por vd
df_val %>% ggplot(aes(x = prob, fill = vd)) +
  geom_density(alpha = .7) +
  geom_vline(xintercept = f_ks$.threshold) +
  scale_fill_grey() + theme_classic() +
  NULL


# importancia de variables ----------------------------------------------------

imp = importance(modelos_bm[["modelo"]][[1]]) %>% sort(decreasing=T)
imp = data.frame(var = names(imp), importance = imp)

imp %>% head(20)
