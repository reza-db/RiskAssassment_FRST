##############################################################
## A.1 Example: Basic concepts of rough set theory
##############################################################
## Using hiring data set, see RoughSetData
data(RoughSetData)
decision.table <- RoughSetData$hiring.dt
## define considered attributes which are first, second, and
## third attributes
attr.P <- c(1,2,3)
## compute indiscernibility relation
IND <- BC.IND.relation.RST(decision.table, feature.set = attr.P)
## compute lower and upper approximations
roughset <- BC.LU.approximation.RST(decision.table, IND)
## Determine regions
region.RST <- BC.positive.reg.RST(decision.table, roughset)
## The decision-relative discernibility matrix and reduct
disc.mat <- BC.discernibility.mat.RST(decision.table, range.object = NULL)