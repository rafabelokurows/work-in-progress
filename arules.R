pacman::p_load(arules,arulesSequences,arulesViz,rattle,RGtk2)
getwd()

teste = read.csv("TESTE.csv",colClasses = "factor",na.strings = "")
trans<-as(teste[,2:6],"transactions")
str(teste)
teste2 = apriori(trans,parameter = list(support = .1,confidence=0.8,minlen=2))
inspect(teste2)
fsets <- apriori(trans, parameter = list(support = .3,confidence=0.8,minlen=1, target = "frequent itemsets"))
inspect(fsets)
inspect(fsets[is.closed(fsets)])
inspect(fsets[is.maximal(fsets)])

load("titanic.raw.rdata")

#https://cran.r-project.org/web/packages/arules/vignettes/arules.pdf
#página 22 - coercing data frame to transactions object


rules <- apriori(titanic.raw)
inspect(rules)

rules2 <- apriori(titanic.raw, parameter = list(minlen=2),appearance = list(rhs=c("Survived=No", "Survived=Yes"), default="lhs"))
rules.sorted <- sort(rules2, by="lift")
inspect(rules.sorted)

teste3 = arules::eclat(teste)
inspect(teste3)

teste <- as(titanic.raw, "transactions")
teste

teste2 = apriori(teste)
rules.sorted <- sort(teste2, by="lift")
inspect(rules.sorted)


data("AdultUCI")
dim(AdultUCI)
str(AdultUCI)

#arulesViz
#https://cran.r-project.org/web/packages/arulesViz/vignettes/arulesViz.pdf

data("Groceries")
str(Groceries)
summary(Groceries)

rules <- apriori(Groceries, parameter=list(support=0.001, confidence=0.5))
inspect(rules)
inspect(head(rules, n = 3, by ="lift"))
plot(rules)
head(quality(rules))
plot(rules, measure = c("support", "lift"), shading = "confidence")
plot(rules, method = "two-key plot")
sel <- plot(rules, measure=c("support", "lift"), shading = "confidence", interactive = TRUE)

subrules <- rules[quality(rules)$confidence > 0.8]
subrules
plot(subrules, method = "matrix", measure = "lift")
plot(subrules, method = "matrix3D", measure = "lift")

plot(rules, method = "grouped")

plot(rules, method = "grouped", control = list(k = 50))
sel <- plot(rules, method = "grouped", interactive = TRUE)
subrules2 <- head(rules, n = 10, by = "lift")
plot(subrules2, method = "graph")
saveAsGraph(head(rules, n = 1000, by = "lift"), file = "rules.graphml")
plot(subrules2, method = "paracoord")

oneRule <- sample(rules, 1)
inspect(oneRule)
plot(oneRule, method = "doubledecker", data = Groceries)


rattle()
