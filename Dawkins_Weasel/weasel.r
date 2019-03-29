# str <- "METHINKS IT IS LIKE A WEASEL"

library(stringi)
str1 <- stri_rand_strings(1, 28, pattern = "[A-Z]")
str2 <- stri_rand_strings(1, 28, pattern = "[A-Z]")
str3 <- stri_rand_strings(1, 28, pattern = "[A-Z]")
str4 <- stri_rand_strings(1, 28, pattern = "[A-Z]")
vect <- stri_rand_strings(100, 28, pattern = "[A-Z]")
# print(str1, str2)

score <- function(a, b){
	empt <- c()
	second <- unlist(strsplit(b, ''))
	for (i in a){
		first <- unlist(strsplit(i, ''))
		logic_vector <- first == second
		empt <- append(empt, sum(logic_vector))
		# print(first)
		# print(second)
	}	
	return(empt/28)
}

mutate <- function(a){
	empt <- c()
	lettersspc <- append(LETTERS, ' ')
	for (a_part in a){
		a_part <- unlist(strsplit(a_part, ''))
		for (j in seq(a_part)){
			prob <- runif(1)
			if (prob < 0.05){
				a_part[j] <- sample(lettersspc, 1)
			}
		}
		empt <- append(empt, paste((a_part), collapse=''))
	}
	return(empt)
}

generation <- function(curBest, mutator, scorer, population){
	v <- rep(curBest, population)
	v_mut <- mutate(v)
	v_score <- score(v_mut, "METHINKS IT IS LIKE A WEASEL")
	newBest <- v_mut[which(v_score == max(v_score))[1]]
	scores <- sort(v_score, decreasing = TRUE)
	return(list(newBest, scores))
}

weasel <- function(nGen=100, target="METHINKS IT IS LIKE A WEASEL", scorer=score, mutator=mutate){
	str11 <- stri_rand_strings(1, nchar(target), pattern = "[A-Z]")
	cBest <- generation(str11, mutate(), score(), 100)[1]
	scoreHistory <- matrix(unlist(generation(str11, mutate(), score(), 100)[2]))
	for (i in seq(nGen)){
		lsBest <- generation(cBest, mutate(), score(), 100)
		cBest <- lsBest[1]
		message("Maximum score of ", i, " generation: ", max(unlist(lsBest[2])))
		message("Average score of ", i, " generation: ", mean(unlist(lsBest[2])), "\n")
		message("String: ", cBest)
		message(sprintf("Average (sprintf) score of %i generation: %f ", i, mean(unlist(lsBest[2]))))
		scoreHistory <- cbind(scoreHistory, unlist(lsBest[2])) 
	}

	return(list(cBest, scoreHistory))
}


weasel(target="METHINKS IT IS LIKE A WEASEL")[1]