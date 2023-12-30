## Entering some data and library activation
library(Rcpp)
library(RoughSets)
library(RoughSetKnowledgeReduction)

## Import Dataset
library(readxl)
SeaportRiskDataset <- read_excel("SeaportRiskDataset.xlsx", 
                                 sheet = "SeaportRiskFactors")
CategoricalDataset <- read_excel("SeaportRiskDataset.xlsx", 
                                 sheet = "Categorical")
DemographicDataset <- read_excel("SeaportRiskDataset.xlsx", 
                                 sheet = "DemographData")

## Load the data
dataset <- SeaportRiskDataset

## Shuffle the data with set.seed
set.seed(5)
dt.Shuffled <- dataset[sample(nrow(dataset)),]

## Split the data into training and testing
idx <- round(0.8 * nrow(dt.Shuffled))
risk.tra <-SF.asDecisionTable(dt.Shuffled[1:idx,],
                              decision.attr = 62, indx.nominal = 62)
risk.tst <- SF.asDecisionTable(dt.Shuffled[
  (idx+1):nrow(dt.Shuffled), -ncol(dt.Shuffled)])

## FEATURE SELECTION
reduct <- FS.feature.subset.computation(risk.tra,
                                        method = "quickreduct.frst")
print(reduct)

## generate new decision tables
risk.tra.fs <- SF.applyDecTable(risk.tra, reduct)
risk.tst.fs <- SF.applyDecTable(risk.tst, reduct)

## INSTANCE SELECTION
indx <- IS.FRIS.FRST(risk.tra.fs,
                     control = list(threshold.tau = 0.3, alpha = 1))

## generate a new decision table
risk.tra.is <- SF.applyDecTable(risk.tra.fs, indx)

## RULE INDUCTION (Rule-based classifiers)
control.ri <- list(
  type.aggregation = c("t.tnorm", "lukasiewicz"),
  type.relation = c("tolerance", "eq.3"),
  t.implicator = "kleene_dienes")
decRules.hybrid <- RI.hybridFS.FRST(risk.tra.is,
                                    control.ri)
summary(decRules.hybrid)
print.listof(decRules.hybrid)
print(decRules.hybrid)

## predicting newdata
predValues.hybrid <- predict(decRules.hybrid,
                             risk.tst.fs)
print(predValues.hybrid)
