# PERFORMANCE ---------------------------------------------------------------------

# KS
metric_ks = function(obs, prob_pred) {
  dat = data.frame(obs = as.factor(obs), pred = prob_pred)
  dat_out = roc_curve(dat, obs, pred)
  fpr = 1 - dat_out$specificity
  tpr = dat_out$sensitivity
  ks = max(tpr - fpr) * 100
  return(ks)
}

# AUC
metric_auc = function(obs, prob_pred) {
  dat = data.frame(obs = as.factor(obs), pred = prob_pred)
  dat_out = roc_auc(dat, obs, pred)
  auc = dat_out[['.estimate']] * 100
  return(auc)
}

# GINI
metric_gini = function(obs, prob_pred) {
  dat = data.frame(obs = as.factor(obs), pred = prob_pred)
  dat_out = gain_capture(dat, obs, pred)
  gini = dat_out[['.estimate']] * 100
  return(gini)
}

# METRICAS DE PERFORMANCE
metrics = function(obs, prob_pred) {
  dat = data.frame(obs=obs, pred=prob_pred) %>% 
    arrange(-pred) %>% 
    mutate(cumbad = cumsum(obs)/sum(obs) * 100)
  # KS
  temp = roc_curve(dat, factor(obs), pred)
  fpr = 1 - temp$specificity
  tpr = temp$sensitivity
  ks = max(tpr - fpr) * 100  
  # GINI
  gini = gain_capture_vec(factor(obs), prob_pred) * 100
  # AUC
  auc = roc_auc_vec(factor(obs), prob_pred) * 100
  # CAPTURES
  qts = quantile(dat$pred, c(0.9, 0.8, 0.7))
  cpt_10 = dat %>% filter(pred > qts[1]) %>% pull(cumbad) %>% tail(1)
  cpt_20 = dat %>% filter(pred > qts[2]) %>% pull(cumbad) %>% tail(1)
  cpt_30 = dat %>% filter(pred > qts[3]) %>% pull(cumbad) %>% tail(1)
  out = data.frame(
    gini=gini, ks=ks, auc=auc, badcap10=cpt_10, badcap20=cpt_20, badcap30=cpt_30
  )
  return(out)
}
