setwd("C:/Users/enerc/OneDrive/Desktop/data science/sessions/r_training")
getwd()

# install.packages("RColorBrewer")
library(arules)
library(arulesViz)
library(RColorBrewer)
library(dplyr)

market_basket <- read.transactions(
  file = "market_basket.csv",
  sep = ",",
  quote = "",
  format = "basket",
  rm.duplicates = TRUE,
  skip = 1
)


# A) Understand the Transactions 
# a. Find the total number of transactions 
# b. Find the total number of items in the inventory 
# c. Find the total number of items purchased 
# d. Find out the 10 most frequently bought items & make a plot

summary(market_basket)

18440 * 22346 *0.0009915565 # To find total number of item purchased we have to multiply(transaction with item and density)

market_basket %>% head(n = 5) %>% inspect

itemFrequencyPlot(x = market_basket,
                  topN = 10,
                  type = "absolute",
                  horiz = TRUE,
                  col = brewer.pal(10,'Paired')
                  )

?brewer.pal

# B) Building 1st set of association rules 
# a. Build apriori algorithm with support value-> 0.005 & Confidence value-> 0.8 
# b. Sort the rules w.r.t confidence & inspect the top 5 rules & the bottom 5 rules 
# c. Sort the rules w.r.t lift & Inspect the top 5 rules 
# d. Plot the rules using different methods

rule1 <- apriori(market_basket, parameter = list(support = 0.005, confidence = 0.8)) 
rule1 <- rule1 %>% sort(by = "confidence")

summary(rule1)

rule1 %>% head(n = 5) %>% inspect
rule1 %>% tail(n = 5) %>% inspect

rule1 <- rule1 %>% sort(by = "lift")
rule1 %>% head(n = 5) %>% inspect

plot(rule1, engine = "htmlwidget")
plot(rule1, method = "two-key", engine = "htmlwidget")
plot(rule1, method = "graph", engine = "htmlwidget")

# C) Building 2nd set of association rules 
# a. Build apriori algorithm with support value-> 0.009 & Confidence value-> 0.3 
# b. Sort the rules w.r.t confidence & inspect the top 5 rules & the bottom 5 rules 
# c. Plot the rules using different methods

rule2 <- apriori(market_basket, parameter = list(support = 0.009, confidence = 0.3)) 
rule2 <- rule2 %>% sort(by = "confidence")

summary(rule2)

rule2 %>% head(n = 5) %>% inspect
rule2 %>% tail(n = 5) %>% inspect

plot(rule2, engine = "htmlwidget")
plot(rule2, method = "two-key", engine = "htmlwidget")
plot(rule2, method = "graph", engine = "htmlwidget")


# D) Building 3rd set of association rules 
# a. Build apriori algorithm with support value-> 0.02 & Confidence value-> 0.5 
# b. Sort the rules w.r.t support & inspect the top 5 rules & the bottom 5 rules 
# c. Plot the rules using different methods

rule3 <- apriori(market_basket, parameter = list(support = 0.02, confidence = 0.5)) 
rule3 <- rule2 %>% sort(by = "support")

summary(rule3)

rule3 %>% head(n = 5) %>% inspect
rule3 %>% tail(n = 5) %>% inspect

plot(rule3, engine = "htmlwidget")
plot(rule3, method = "two-key", engine = "htmlwidget")
plot(rule3, method = "graph", engine = "htmlwidget")
