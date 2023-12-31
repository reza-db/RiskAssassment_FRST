###############################################################
## B Example : Data analysis based on RST and FRST
## In this example, we are using wine dataset for both RST and FRST
###############################################################
## Load the data
## Not run: data(RoughSetData)
dataset <- RoughSetData$wine.dt

## Shuffle the data with set.seed
set.seed(5)
dt.Shuffled <- dataset[sample(nrow(dataset)),]

## Split the data into training and testing
idx <- round(0.8 * nrow(dt.Shuffled))
wine.tra <-SF.asDecisionTable(dt.Shuffled[1:idx,],
                              decision.attr = 14, indx.nominal = 14)
wine.tst <- SF.asDecisionTable(dt.Shuffled[
  (idx+1):nrow(dt.Shuffled), -ncol(dt.Shuffled)])

## DISCRETIZATION
cut.values <- D.discretization.RST(wine.tra,
                                   type.method = "global.discernibility")
d.tra <- SF.applyDecTable(wine.tra, cut.values)
d.tst <- SF.applyDecTable(wine.tst, cut.values)

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

#################################################
## Examples: Data analysis using the wine dataset
## 2. Learning and prediction using FRST
#################################################
## FEATURE SELECTION
reduct <- FS.feature.subset.computation(wine.tra,
                                        method = "quickreduct.frst")
print(reduct)

## generate new decision tables
wine.tra.fs <- SF.applyDecTable(wine.tra, reduct)
wine.tst.fs <- SF.applyDecTable(wine.tst, reduct)

## INSTANCE SELECTION
indx <- IS.FRIS.FRST(wine.tra.fs,
                     control = list(threshold.tau = 0.3, alpha = 1))

## generate a new decision table
wine.tra.is <- SF.applyDecTable(wine.tra.fs, indx)

## RULE INDUCTION (Rule-based classifiers)
control.ri <- list(
  type.aggregation = c("t.tnorm", "lukasiewicz"),
  type.relation = c("tolerance", "eq.3"),
  t.implicator = "kleene_dienes")
decRules.hybrid <- RI.hybridFS.FRST(wine.tra.is,
                                    control.ri)
summary(decRules.hybrid)
print.listof(decRules.hybrid)
print(decRules.hybrid)

## predicting newdata
predValues.hybrid <- predict(decRules.hybrid,
                             wine.tst.fs)
print(predValues.hybrid)

#################################################
## Examples: Data analysis using the wine dataset
## 3. Prediction using fuzzy nearest neighbor classifiers
#################################################
## using FRNN.O
control.frnn.o <- list(m = 2,
                       type.membership = "gradual")
predValues.frnn.o <- C.FRNN.O.FRST(wine.tra.is,
                                   newdata = wine.tst.fs, control = control.frnn.o)

## Using FRNN
control.frnn <- list(type.LU = "implicator.tnorm",k=20,
                     type.aggregation = c("t.tnorm", "lukasiewicz"),
                     type.relation = c("tolerance", "eq.1"),
                     t.implicator = "lukasiewicz")
predValues.frnn <- C.FRNN.FRST(wine.tra.is,
                               newdata = wine.tst.fs, control = control.frnn)

## calculating error
real.val <- dt.Shuffled[(idx+1):nrow(dt.Shuffled),
                        ncol(dt.Shuffled), drop = FALSE]
err.1 <- 100*sum(pred.vals!=real.val)/nrow(pred.vals)
err.2 <- 100*sum(predValues.hybrid!=real.val)/
  nrow(predValues.hybrid)
err.3 <- 100*sum(predValues.frnn.o!=real.val)/
  nrow(predValues.frnn.o)
err.4 <- 100*sum(predValues.frnn!=real.val)/
  nrow(predValues.frnn)
cat("The percentage error = ", err.1, "\n")
cat("The percentage error = ", err.2, "\n")
cat("The percentage error = ", err.3, "\n")
cat("The percentage error = ", err.4, "\n")
## End(Not run)