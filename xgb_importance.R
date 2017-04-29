require(xgboost)
require(Matrix)
require(data.table)
if (!require('vcd')) install.packages('vcd')

data(Arthritis)
df <- data.table(Arthritis, keep.rownames = F)

head(df)

head(df[,AgeDiscret := as.factor(round(Age/10,0))])
head(df[,AgeCat:= as.factor(ifelse(Age > 30, "Old", "Young"))])

sparse_matrix <- sparse.model.matrix(Improved~.-1, data = df)
head(sparse_matrix)

output_vector = df[,Improved] == "Marked"

bst <- xgboost(data = sparse_matrix, label = output_vector, max.depth = 4,
               eta = 1, nthread = 2, nround = 10,objective = "binary:logistic")

importance <- xgb.importance(feature_names = sparse_matrix@Dimnames[[2]], model = bst)
head(importance)
