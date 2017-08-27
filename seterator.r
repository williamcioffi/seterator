#set simulation: THE SETERATOR

ncards <- 81
nattributes <- 4
nperattributes <- 3

cards <- as.matrix(expand.grid(1:3, 1:3, 1:3, 1:3))
colnames(cards) <- 1:4
# image(cards, col = rainbow(3))




nsim <- 10000
endingnum <- vector()
pb <- txtProgressBar(style = 3)
for(q in 1:nsim) {
setTxtProgressBar(pb, q /nsim)
#starttime <- Sys.time()

#start of play with a full deck
deck <- sample(1:ncards, ncards, replace = FALSE)
ask <- 12
deal <- vector()
hand <- vector()
start <- 1
cardsleft <- length(deck)
anysets <- TRUE

#kk <- 1

while(cardsleft > 0 || anysets) {
	if(ask == 0 & length(hand) == 0) {
		break
	}else if(ask != 0) {
		cardsleft <- cardsleft - ask
		end <- start + ask - 1
		deal <- deck[start:end]
	
		start <- end + 1
		hand <- rbind(hand, cards[deal,])
	}
	
	nhand <- nrow(hand)
	
	posscompare <- t(combn(1:nhand, 3))
	sets <- list()
	kount <- 1
# pb <- txtProgressBar(style = 3)
	for(i in 1:nrow(posscompare)) {
# setTxtProgressBar(pb, i / nrow(posscompare))
		tocomp <- hand[posscompare[i,],]
		u_tocomp <- apply(tocomp, 2, unique)
		l_tocomp <- lapply(u_tocomp, length)
		nummatches <- length(which(l_tocomp == 1 | l_tocomp == 3))
		
		if(nummatches == 4) {
			sets[[kount]] <- posscompare[i,]
			kount <- kount + 1
		}
	}
# close(pb)
	
	if(length(sets) == 0) {
		anysets <- FALSE
		if(cardsleft > 0) {
			ask <- 3
		} else {
			ask <- 0
		}
	} else if(length(sets) > 0) {
		anysets <- TRUE
		hand <- hand[-sets[[1]], ]
		
		if(nrow(hand) == 9 && cardsleft > 0) {
			ask <- 3
		} else {
			ask <- 0
		}
	}

#kk <- kk + 1
}

endingnum[q] <- nrow(hand)
#diff <- Sys.time() - starttime
#print(diff)
}

close(pb)