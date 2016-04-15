data <- data("ToothGrowth")
qplot(dose, data = ToothGrowth, geom = "histogram", 
      weight = len, color = supp, facets = .~supp)
qplot(as.factor(dose), len, data = ToothGrowth, facets = .~supp)
qplot(dose, len, data = ToothGrowth, facets = dose~supp)
qplot(dose, data = ToothGrowth, weight = len, geom = "histogram",
      facets = dose~supp,  fill = supp)


qplot(tootalGroeth, data = a, geom = "bar", facets = .~supp)


total <- ddply(ToothGrowth, .(supp, dose), summarise, 
               meanGrowth = mean(len), totlaGrowth = sum(len))
p <- ggplot(total, aes(as.factor(dose), meanGrowth, fill = supp)) + 
       geom_bar(stat = "identity", position = "dodge")
p
