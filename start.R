data <- data("ToothGrowth")
qplot(dose, data = ToothGrowth, geom = "histogram", 
      weight = len, color = supp, facets = .~supp)
qplot(as.factor(dose), len, data = ToothGrowth, facets = .~supp)
qplot(dose, len, data = ToothGrowth, facets = dose~supp)
qplot(dose, data = ToothGrowth, weight = len, geom = "histogram",
      facets = dose~supp,  fill = supp)

#use this. compare effect of multiple dises of each supply mean
total <- ddply(ToothGrowth, .(supp, dose), summarise, 
               meanGrowth = mean(len), totlaGrowth = sum(len))
p <- ggplot(total, aes(as.factor(dose), meanGrowth, fill = supp)) + 
       geom_line(stat = "identity", position = "dodge")
p <- ggplot(total, aes(dose, meanGrowth, color = supp)) + 
       geom_line(stat = "identity", size = 1)
p


hist(ToothGrowth$len[ToothGrowth$supp == "VC"])
hist(ToothGrowth$len[ToothGrowth$supp == "OJ"])
#parametric test using t-confidence intervals
summary(ToothGrowth)
t.test(ToothGrowth$len[ToothGrowth$supp == "OJ"], 
       ToothGrowth$len[ToothGrowth$supp == "VC"], paired = F)
t.test(ToothGrowth$len[ToothGrowth$supp == "OJ" & ToothGrowth$dose == 0.5], 
       ToothGrowth$len[ToothGrowth$supp == "VC" & ToothGrowth$dose == 0.5], paired = F)
t.test(ToothGrowth$len[ToothGrowth$supp == "OJ" & ToothGrowth$dose == 1], 
       ToothGrowth$len[ToothGrowth$supp == "VC" & ToothGrowth$dose == 1], paired = F)
t.test(ToothGrowth$len[ToothGrowth$supp == "OJ" & ToothGrowth$dose == 2], 
       ToothGrowth$len[ToothGrowth$supp == "VC" & ToothGrowth$dose == 2], paired = F)


#non-parametric test
testStat <- function(w, g) mean(w[g == "OJ" ]) - mean(w[g == "VC"])
testStat <- function(w, g, t) mean(w[g == "OJ" & ToothGrowth$dose == t ]) -
                            mean(w[g == "VC" & ToothGrowth$dose == t])
observedstat <- testStat(ToothGrowth$len, ToothGrowth$supp, 2)
permutations <- sapply(1:10000, 
                       function(i) testStat(ToothGrowth$len, 
                                            sample(ToothGrowth$supp), 2))
observedstat

mean(permutations > observedstat)

hist(permutations)
abline(v = observedstat)
