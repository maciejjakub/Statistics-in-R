library(mlbench)
data(Sonar)

Y <- Sonar[["Class"]]
B <- Sonar[names(Sonar) != "Class"]
testvec <- runif(60)
testmx <- matrix(runif(20*60), nrow = 20, ncol = 60)

train <- B[c(T, F), ] #select odd rows
test <- B[c(F, T), ] #select even rows


distance <- function(vect, mx){
	dis <- apply(mx, 1, function(x) sqrt(sum((vect - x)^2))) 
	sort(dis)
}

kNNsingle <- function(a, B, Y, k){
	sorted <- distance(a, B)[1:k]
	vals <- Y[as.numeric(names(sorted))]
	final_type <- names(sort(table(vals), decreasing=TRUE)[1])
}

kNN <- function(trainX, trainY, testX, k=5){
	apply(testX, 1, function(x) kNNsingle(x, trainX, trainY, k))
}

# print(head(distance(testvec, B)))

# print(kNNsingle(testvec, B, Y, 7))

# print(dim(testmx))
# print(kNN(B, Y, testmx))

# print(head(distance(B[2, ], train)))
res <- kNN(train, Y, test)
print(res)
print(res == Y[c(F, T)])
print(sum(res == Y[c(F, T)])/104)

# print(kNNsingle(B[3, ], B, Y, 5))
