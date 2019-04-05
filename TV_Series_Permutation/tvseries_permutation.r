load('seriale.RData')

len <- sapply(seriale, length)
mdn <- sapply(seriale, median)
quantl25 <- sapply(seriale, function(x) quantile(x, .25, names = FALSE))
quantl75 <- sapply(seriale, function(x) quantile(x, .75, names = FALSE))
min_val <- sapply(seriale, min)
max_val <- sapply(seriale, max)

message('Longest TV Series: ', attributes(which.max(len)), '\n', 
	'Number of episodes: ', max(len), '\n\n',
	'Highest Rated TV Series: ', attributes(which.max(mdn)), '\n',
	'Rates median: ', max(mdn), '\n\n',
	'TV Series with biggest rates difference: ', attributes(which.max(quantl75 - quantl25)), '\n',
	'Difference between 75% quantile and 25% quantile: ', max(quantl75 - quantl25), '\n')