##############################################################
## A.2 Example: Basic concepts of fuzzy rough set theory
##############################################################
## Using pima7 data set, see RoughSetData
data(RoughSetData)
decision.table <- RoughSetData$pima7.dt
## In this case, let us consider the first and second attributes
conditional.attr <- c(1, 2)
## We are using the "lukasiewicz" t-norm and the "tolerance" relation
## with "eq.1" as fuzzy similarity equation
control.ind <- list(type.aggregation = c("t.tnorm", "lukasiewicz"),
                    type.relation = c("tolerance", "eq.1"))
## Compute fuzzy indiscernibility relation
IND.condAttr <- BC.IND.relation.FRST(decision.table, attributes = conditional.attr,
                                     control = control.ind)
## Compute fuzzy lower and upper approximation using type.LU : "implicator.tnorm"
## Define index of decision attribute
decision.attr = c(9)
## Compute fuzzy indiscernibility relation of decision attribute
## We are using "crisp" for type of aggregation and type of relation
control.dec <- list(type.aggregation = c("crisp"), type.relation = "crisp")
IND.decAttr <- BC.IND.relation.FRST(decision.table, attributes = decision.attr,
                                    control = control.dec)
## Define control parameter containing type of implicator and t-norm
control <- list(t.implicator = "lukasiewicz", t.tnorm = "lukasiewicz")
## Compute fuzzy lower and upper approximation
FRST.LU <- BC.LU.approximation.FRST(decision.table, IND.condAttr, IND.decAttr,
                                    type.LU = "implicator.tnorm", control = control)
## Determine fuzzy positive region and its degree of dependency
fuzzy.region <- BC.positive.reg.FRST(decision.table, FRST.LU)