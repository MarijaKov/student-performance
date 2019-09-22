#evaluacione metrike
getEvaluationMetrics <- function(cmatrix) {
  TP <- cmatrix[2,2] # true positive
  TN <- cmatrix[1,1] # true negative
  FP <- cmatrix[1,2] # false positive
  FN <- cmatrix[2,1] # false negative
  acc <- sum(diag(cmatrix)) / sum(cmatrix)
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  F1 <- 2*precision*recall / (precision + recall)
  c(accuracy = acc, precision = precision, recall = recall, F1 = F1)
}
#Pozitivna klasa je "YES"