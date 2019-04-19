load('seriale.RData')

len <- sapply(seriale, length)
mdn <- sapply(seriale, median)
meann <- sapply(seriale, mean)
quantl25 <- sapply(seriale, function(x) quantile(x, .25, names = FALSE))
quantl75 <- sapply(seriale, function(x) quantile(x, .75, names = FALSE))
min_val <- sapply(seriale, min)
max_val <- sapply(seriale, max)

message('Longest TV Series: ', attributes(which.max(len)), '\n', 
	'Number of episodes: ', max(len), '\n\n',
	'Highest Rated TV Series: ', attributes(which.max(mdn)), '\n',
	'Rates median: ', max(mdn), '\n\n',
	'TV Series with biggest rates difference: ', attributes(which.max(quantl75 - quantl25)), '\n',
	'Difference between 75% quantile and 25% quantile: ', max(quantl75 - quantl25), '\n\n',
	'Spearman corellation between length and median rate: ', cor(len, mdn, m = "s"), '\n',
	'Spearman corellation between length and 25% quantile: ', cor(len, quantl25, m = "s"), '\n', 
	'Spearman corellation between length and 75% quantile: ', cor(len, quantl75, m = "s"), '\n',
	'Spearman corellation between length and rates dispersal: ', cor(len, quantl75 - quantl25, m = "s"), '\n')

# plot(len, quantl75 - quantl25)

permutation_test <- function(a, b){
	cor1 <- cor(a, b, m = "s")
	repl <- replicate(10000, cor(sample(a), b, m = "s"))
	return(sum(cor1 < repl)/length(repl))
}

# print(permutation_test(len, quantl75 - quantl25))

second_permutation_test <- function(a, b){
	first <- unlist(unname(a))
	second <- unlist(unname(b))
	median_diff <- abs(median(first) - median(second))
	repl <- replicate(10000, {
		lss <- split(sample(c(first, second)), rep(c(names(a), names(b)), c(length(first), length(second))))
		med_diff <- abs(median(unlist(lss[1])) - median(unlist(lss[2])))
		})
	return(sum(median_diff < repl)/length(repl))
}


print(second_permutation_test(seriale[47], seriale[12]))