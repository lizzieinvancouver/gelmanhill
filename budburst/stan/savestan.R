savestan <- function() {
	tosave <- which(
		sapply(ls(), function(x) class(get(x)))
				=="stanfit" |
		  sapply(ls(), function(x) class(get(x)))
		=="shinystan" 
		  )
	save(file=paste("Stan Output ", Sys.Date(), ".RData", sep=""), list = ls()[tosave])
	} 