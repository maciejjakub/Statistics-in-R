library(stringi)

score <- function(a, b){
	mx <- sapply(a, function(x) sum(unlist(strsplit(x, '')) == unlist(strsplit(b, ''))))
	return(unname(mx/nchar(a)))
}

mutate <- function(a){
	letters_spc <- c(LETTERS, ' ')
	a <- sapply(a, function(x){
		strr <- unlist(strsplit(x, ''))
		prob <- runif(nchar(x))
		strr[prob <= 0.05] <- sample(letters_spc, length(which(prob <= 0.05)), replace = TRUE)
		paste(strr, collapse = '')
		})
	return(unname(a))
}

generation <- function(curBest, mutator, scorer, population, tgt){
	v <- rep(curBest, population)
	v_mut <- mutate(v)
	v_score <- score(v_mut, tgt)
	newBest <- v_mut[which(v_score == max(v_score))[1]]
	scores <- sort(v_score, decreasing = TRUE)
	return(list(newBest, scores))
}

weasel <- function(nGen=100, target="METHINKS IT IS LIKE A WEASEL", scorer=score, mutator=mutate){
	str11 <- stri_rand_strings(1, nchar(target), pattern = "[A-Z]")
	cBest <- generation(str11, mutate(), score(), 100, target)[1]
	scoreHistory <- matrix(unlist(generation(str11, mutate(), score(), 100, target)[2]))
	for (i in seq(nGen)){
		lsBest <- generation(cBest, mutate(), score(), 100, target)
		cBest <- lsBest[1]
		message("String: ", cBest)
		message("Maximum score of ", i, " generation: ", max(unlist(lsBest[2])))
		message("Average score of ", i, " generation: ", mean(unlist(lsBest[2])))
		message(sprintf("Average (sprintf) score of %i generation: %f \n", i, mean(unlist(lsBest[2]))))
		scoreHistory <- cbind(scoreHistory, unlist(lsBest[2]))
	}

	return(list(cBest, scoreHistory))
}


weasel(target="METHINKS IT IS LIKE A WEASEL")[1]