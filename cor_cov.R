# There’s always at least two ways to do the same thing: an example generating 3-level hierarchical data using simstudy
# https://www.r-bloggers.com/theres-always-at-least-two-ways-to-do-the-same-thing-an-example-generating-3-level-hierarchical-data-using-simstudy/


rm(list=ls())


library(simstudy)
library(data.table)

### Group defintion

defg <- defData(varname = "gamma", formula=0, variance = 2, id = "cid")

### Individal definition

defi <- defDataAdd(varname = "alpha", formula = 0, variance = 1.3)

### Outcome definition

defC <- defCondition(condition = "period == 0", 
                     formula = "3 + gamma + alpha",
                     dist = "nonrandom")
defC <- defCondition(defC, condition = "period == 1", 
                     formula = "4 + gamma + alpha",
                     dist = "nonrandom")
defC <- defCondition(defC, condition = "period == 2", 
                     formula = "6 + gamma + alpha",
                     dist = "nonrandom")

defy <- defDataAdd(varname = "y", formula = "mu", variance = 1.1)

set.seed(3483)
dgrp1 <- genData(100, defg)

dind1 <- genCluster(dgrp1, "cid", numIndsVar = 20, level1ID = "id")
dind1 <- addColumns(defi, dind1)

dper1 <- addPeriods(dind1, nPeriods = 3, idvars = "id")
dper1 <- addCondition(defC, dper1, newvar = "mu")

dper1 <- addColumns(defy, dper1)

dcor1 <- dcast(dper1, id + cid ~ period, value.var = "y")
setnames(dcor1, c("id", "cid", "y0", "y1", "y2"))

dcor1[, cov(cbind(y0, y1, y2))]

dcor1[, cor(cbind(y0, y1, y2))]

defg <- defData(varname = "gamma", 
                formula = 0, variance = 2, id = "cid")
defg <- defData(defg, varname = "mu", 
                formula = 0, dist = "nonrandom")
defg <- defData(defg, varname = "phi", 
                formula = 2.4, dist = "nonrandom")

defC <- defCondition(condition = "period == 0", 
                     formula = "3 + gamma + e",
                     dist = "nonrandom")
defC <- defCondition(defC, condition = "period == 1", 
                     formula = "4 + gamma + e",
                     dist = "nonrandom")
defC <- defCondition(defC, condition = "period == 2", 
                     formula = "6 + gamma + e",
                     dist = "nonrandom")

set.seed(3483)
dgrp2 <- genData(100, defg)

dind2 <- genCluster(dgrp2, "cid", numIndsVar = 20, level1ID = "id")

dper2 <- addPeriods(dind2, nPeriods = 3, idvars = "id")
dper2 <- addCorGen(dper2, "id", nvars = 3, param1 = "mu", param2 = "phi",
                   rho = .54167, dist = "normal", corstr = "cs", cnames = "e")
dper2 <- addCondition(defC, dper2, newvar = "y")

dcor2 <- dcast(dper2, id + cid ~ period, value.var = "y")
setnames(dcor2, c("id", "cid", "y0", "y1", "y2"))

dcor2[, cov(cbind(y0, y1, y2))]

dcor2[, cor(cbind(y0, y1, y2))]
