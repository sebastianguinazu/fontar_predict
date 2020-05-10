### funciones

# pega strings -------------------------------------------------------------------

"%+%" = function(a,b) paste(a, b, sep = "")


# para setear max en 1 en vars de prop -------------------------------------------

mod_vars = function(bds, vars) {
  for (var in vars) {
    to_mod = bds %>% select(starts_with(var)) %>% colnames()
    bds = bds %>% mutate_at(to_mod, function(x) {ifelse(x>1, 0, x)})
  }
  return(bds)
}

# para crear variables trend -----------------------------------------------------

var_trend = function(bds, vars) {
  for_trend_f = map(c("_trend", "1yearb", "2yearb", "3yearb"), ~ vars %+% .)
  for (i in 1:length(vars)){
    bds[for_trend_f[[1]][i]] = bds[for_trend_f[[2]][i]] / bds[for_trend_f[[3]][i]]
  }
  return(bds)
}
