iv <- function(data, inputvary){
  
  infvalue <- create_infotables(data=data, y = inputvary,
                                parallel = FALSE)
  data.frame(infvalue$Summary)
}

ig <- function(data, inputvary){
  varx <- setdiff(colnames(data),inputvary)
  infogain <- information.gain(formula = as.formula(paste(inputvary, "~", paste(varx, collapse = "+"))), data = data, unit = 'log2')
  peubah <- rownames(infogain)
  Variable <- peubah[order(infogain, decreasing = TRUE)]
  IG <- infogain[order(infogain, decreasing = TRUE),]
  cbind(Variable, IG)
}

mdgmda <- function(data, inputvary){
  
  Variable <- setdiff(colnames(data),inputvary)
  set.seed(43)
  model.rf <- randomForest(formula = as.formula(paste(inputvary," ~ ",
                                                      paste(Variable,collapse="+"))),
                           data = data, 
                           proximity = TRUE, importance = TRUE, nodesize = 5)
  a <- importance(model.rf)
  b <- a[,3:4]
  data.frame(Variable, b)
}