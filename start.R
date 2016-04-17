data("ToothGrowth")
qplot(dose, data = ToothGrowth, geom = "histogram", 
      weight = len, color = supp, facets = .~supp)
qplot(as.factor(dose), len, data = ToothGrowth, facets = .~supp)
qplot(dose, len, data = ToothGrowth, facets = dose~supp)
qplot(dose, data = ToothGrowth, weight = len, geom = "histogram",
      facets = dose~supp,  fill = supp)


#use this. compare effect of multiple dises of each supply mean
total <- ddply(ToothGrowth, .(supp, dose), summarise, 
               meanGrowth = mean(len))
p <- ggplot(total, aes(as.factor(dose), meanGrowth, fill = supp)) + 
       geom_line(stat = "identity", position = "dodge")
p <- ggplot(total, aes(dose, meanGrowth, color = supp)) + 
       geom_line(stat = "identity", size = 1)
p +  ggtitle("Average length of tooth vs dose of vitamin C ") +
       scale_color_discrete( name  = "Delivary method", labels = c("Orange Juice", "Ascorbic Acid"))

#
qplot(len, data = ToothGrowth, geom = "histogram",facets = supp~dose, fill = as.factor(dose))

hist(ToothGrowth$len[ToothGrowth$supp == "VC"])
hist(ToothGrowth$len[ToothGrowth$supp == "OJ"])
#parametric test using t-confidence intervals
summary(ToothGrowth)
t.test(ToothGrowth$len[ToothGrowth$supp == "OJ"], 
       ToothGrowth$len[ToothGrowth$supp == "VC"], paired = F)
t <- t.test(ToothGrowth$len[ToothGrowth$supp == "OJ" & ToothGrowth$dose == 0.5], 
       ToothGrowth$len[ToothGrowth$supp == "VC" & ToothGrowth$dose == 0.5], paired = F)
t.test(ToothGrowth$len[ToothGrowth$supp == "OJ" & ToothGrowth$dose == 1], 
       ToothGrowth$len[ToothGrowth$supp == "VC" & ToothGrowth$dose == 1], paired = F)
t.test(ToothGrowth$len[ToothGrowth$supp == "OJ" & ToothGrowth$dose == 2], 
       ToothGrowth$len[ToothGrowth$supp == "VC" & ToothGrowth$dose == 2], paired = F)


#non-parametric test
t<- 2
observedstat <- testStat(ToothGrowth$len[ToothGrowth$dose == t], 
                         ToothGrowth$supp[ToothGrowth$dose == t])
permutations <- sapply(1:10000, 
                       function(i) testStat(ToothGrowth$len[ToothGrowth$dose == t],
                                            sample(ToothGrowth$supp[ToothGrowth$dose == t])))
observedstat

mean(permutations > observedstat)

hist(permutations)
abline(v = observedstat)
