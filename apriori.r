# Step 1
library(arules) 
library(arulesViz) 
library(RColorBrewer)

# Step 2
data(Groceries) 
Groceries

summary(Groceries) 
class(Groceries)

# Step 3
rules = apriori(Groceries, parameter = list(supp = 0.02, conf = 0.2))
summary (rules)

# Step 4
inspect(rules[1:10]) 

# Step 5
arules::itemFrequencyPlot(
  Groceries,
  topN = 20,
  col = brewer.pal(8, 'Pastel2'),
  main = 'Relative Item Frequency Plot',
  type = "relative",
  ylab = "Item Frequency (Relative)"
)
itemsets = apriori(Groceries, parameter = list(minlen=2, maxlen=2,support=0.02, target="frequent itemsets")) 
summary(itemsets)

# Step 6
inspect(itemsets[1:10])
itemsets_3 = apriori(Groceries, parameter = list(minlen=3, maxlen=3,support=0.02, target="frequent itemsets")) 
summary(itemsets_3)

# Step 7
inspect(itemsets_3)
