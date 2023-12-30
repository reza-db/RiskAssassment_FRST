## Entering some data and library activation
library(Rcpp)
library(RoughSets)
library(RoughSetKnowledgeReduction)

## Import Dataset
library(readxl)
SeaportRiskDataset <- read_excel("Seaport Risk Assessment with RST/SeaportRiskDataset.xlsx", 
                                 sheet = "SeaportRiskFactors")
CategoricalDataset <- read_excel("Seaport Risk Assessment with RST/SeaportRiskDataset.xlsx", 
                                 sheet = "Categorical")
DemographicDataset <- read_excel("Seaport Risk Assessment with RST/SeaportRiskDataset.xlsx", 
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

## DISCRETIZATION
cut.values <- D.discretization.RST(risk.tra,
                                   type.method = "global.discernibility")
d.tra <- SF.applyDecTable(risk.tra, cut.values)
d.tst <- SF.applyDecTable(risk.tst, cut.values)

## FEATURE SELECTION
red.rst <- FS.feature.subset.computation(d.tra,
                                         method="quickreduct.rst")
fs.tra <- SF.applyDecTable(d.tra, red.rst)
head(fs.tra)

## RULE INDUCTION
rules <- RI.indiscernibilityBasedRules.RST(d.tra,
                                           red.rst)
summary(rules)
print.listof(rules)
print.factor(rules)

## predicting newdata
pred.vals <- predict(rules, d.tst)
print(pred.vals)
