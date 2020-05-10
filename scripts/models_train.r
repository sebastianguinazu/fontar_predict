library(yardstick)
library(ranger)
library(methods)
library(Matrix)
library(xgboost)
library(gbm)


# LOGISTIC ---------------------------------------------------------------------

# train glm
train_lg = function(x, y, semilla =88) {
  dat = bind_cols(x, vd = y)
  set.seed(semilla)
  mod = glm(vd ~ ., family='binomial', data=dat)
  return(mod)
}

# predict probs glm
pred_lg = function(modelo, x) {
  probs = predict(modelo, x, type='response') %>% unname()
  return(probs)
}

# RANGER ------------------------------------------------------------------------

# train ranger
train_rf = function(x, y, 
                    ntrees=300, minobj=5, varstry=NA, semilla=88) {
  dat = bind_cols(x, vd = y) %>% 
    # porque sino modela prob de 0
    mutate(vd = ifelse(vd %in% 1, 0, 1) %>% as.factor())
  set.seed(semilla)
  if (is.na(varstry)) varstry = floor(sqtr(ncol(x)))
  mod = ranger(
    vd ~ .
    ,data = dat
    ,num.trees = ntrees
    ,min.node.size = minobj
    ,probability = T
    ,importance = 'impurity'
    ,respect.unordered.factors = T
  )
}

# predict probs ranger
pred_rf = function(modelo, x) {
  probs = predict(modelo, data = x)$predictions[,1]
  return(probs)
}


# XGBOOST -------------------------------------------------------------------------------

# train XGB
train_xg = function(x, y
                    , nrounds=10, learn_rate=0.3, maxdepth=3, minchildw=1
                    ,subsample=1, colsample=1
                    ,gamma=0, lambda=1, alpha=0
                    ,x_val=NULL, y_val=NULL, semilla=88) {
  dat = bind_cols(x, vd=y)
  X = model.matrix(vd ~ .-1, dat)
  y = dat$vd
  dtrain = xgb.DMatrix(data=X, label=y)
  if ((!is.null(x_val)) & (!is.null(y_val))) {
    dat_val = bind_cols(x_val, vd = y_val)
    X_val = sparse.model.matrix(vd ~ ., dat_val)[,-1]
    y_val = dat_val$vd
    dval = xgb.DMatrix(data=X_val, label=y_val)
    watchlist = list(train=dtrain, test=dval)
    set.seed(semilla)
    mod = xgb.train(
      data = dtrain
      ,nrounds = nrounds
      ,eta = learn_rate
      ,max_depth = maxdepth
      ,min_child_weight = minchildw
      ,subsample = subsample
      ,colsample_bytree = colsample
      ,gamma = gamma
      ,lambda = lambda
      ,alpha = alpha
      ,watchlist = watchlist
      ,objective = 'binary:logistic'
    )
  } else {
    mod = xgb.train (
      data = dtrain
      ,nrounds = nrounds
      ,eta = learn_rate
      ,max_depth = maxdepth
      ,min_child_weight = minchildw
      ,subsample = subsample
      ,colsample_bytree = colsample
      ,gamma = gamma
      ,lambda = lambda
      ,alpha = alpha
      ,objective = 'binary:logistic'
    )
  }
  return(mod)
}

# predict probs xgb
pred_xg = function(modelo, x) {
  dat = as(as.matrix(x), 'dgCMatrix')
  probs = predict(modelo, dat)
  return(probs)
}


# GBM ----------------------------------------------------------------------------------

# train gradient boosting machine
train_gb = function(x, y, 
                    ntrees=1000, learn_rate=0.01, int_depth=5, semilla=88) {
  set.seed(semilla)
  mod = gbm.fit(
    x=x, y=y
    ,distribution = 'bernoulli'
    ,n.trees = ntrees
    ,shrinkage = learn_rate
    ,interaction.depth = int_depth
    ,n.minobsinnode = 10
  )
  return(mod)
}

# predict probs gbm
pred_gb = function(modelo, x) {
  probs = predict(modelo, newdata=x, type='response', n.trees=modelo$n.trees)
  return(probs)
}
