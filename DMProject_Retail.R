install.packages('zoo')
install.packages('arulesCBA')
install.packages('rCBA')
install.packages('rJava')
library('zoo')
library('arulesCBA')
library('arules')
library('rCBA')
rm(list=ls())

retail = read.csv('Online_Retail.csv',
                  sep=',',
                  na.strings=c('', 'N/A'),
                  stringsAsFactors=FALSE)

summary(retail)

retail.top5 <- head(retail, 5)

retail$CustomerID <- na.locf(retail$CustomerID)
retail <- na.omit(retail)
retail <- retail[!duplicated(retail), ]

retail <- retail[retail$Quantity > 0, ]
retail <- retail[retail$UnitPrice > 0, ]

boxplot(retail$Quantity, main = "Boxplot Quantity", ylab = "Quantity", col = "lightblue")
boxplot(retail$UnitPrice, main = "Boxplot Unit Price", ylab = "Unit Price", col = "lightblue")

iqr.unitprice <- IQR(retail$UnitPrice)
q1.unitprice <- quantile(retail$UnitPrice, 0.25)
q3.unitprice <- quantile(retail$UnitPrice, 0.75)

lower_bound_quantity <- q1.unitprice - 1.5 * iqr.unitprice
upper_bound_quantity <- q3.unitprice + 1.5 * iqr.unitprice

retail.preprocessed <- retail[retail$Quantity >= lower_bound_quantity & retail$Quantity <= upper_bound_quantity, ]

boxplot(retail.preprocessed$Quantity, main = "Boxplot Quantity", ylab = "Quantity", col = "lightblue")

retail.transformed <- split(retail.preprocessed$Description,
                            retail.preprocessed$InvoiceNo)

retail.transformed <- as(retail.transformed, 'transactions')


# Apriori Algorithm
frequent.itemsets <- apriori(retail.transformed,
                             parameter=list(
                               supp=0.02,
                               target='frequent itemsets'
                             ))
sorted.itemsets <- sort(frequent.itemsets, by = "support", decreasing = TRUE)
sorted.itemsets <- subset(sorted.itemsets, size(sorted.itemsets) >= 2)
inspect(sorted.itemsets)

rules <- apriori(retail.transformed,
                 parameter=list(
                   supp=0.02,
                   conf=0.6,
                   target='rules'))

sorted.rules <- sort(rules, by = "confidence", decreasing = TRUE)
inspect(sorted.rules)

# ECLAT Algorithm
frequent.itemsets.eclat <- eclat(retail.transformed,
                                 parameter=list(supp = 0.02, 
                                                  maxlen = 10
                                                ))

sorted.itemsets.eclat <- sort(frequent.itemsets.eclat, by = "support", decreasing = TRUE)

inspect(sorted.itemsets.eclat)

rules.eclat <- ruleInduction(sorted.itemsets.eclat,
                             retail.transformed,
                             confidence = 0.6)

sorted.rules.eclat <- sort(rules.eclat, by = "confidence", decreasing = TRUE)

inspect(sorted.rules.eclat)


# FP-Growth Algorithm (jangan lupa dibenerin)
library(arules)
# write(retail.transformed, file='data.csv', sep=',')
# system('cmd', input='fpgrowth -s2 -k, data.csv frequent_itemsets.csv')
# system('cmd', input='fpgrowth -tr -s2 -c60 -k, data.csv rules')