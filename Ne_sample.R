get.sample = function(rland, ID=NULL, sample.size = NULL, nloci = 8) {
	s = sample(which(rland$individuals[,1] %in% c(1,6,23,24,28,29)), sample.size, replace = FALSE);  
	Demedat = matrix(data = NA, nrow = sample.size, ncol = 2*nloci);
	for(x in 1:nloci) {
		temp = landscape.states(rland, lnum = x)[s,7:8];
		if(x == 1) {
			Demedat = temp;
		} else {
			Demedat = cbind(Demedat, temp);
		}
		temp = c();
	}
	
	gendat = matrix(data = NA, nrow = sample.size, ncol = nloci);
	allpos = seq(from = 1, length.out = nloci, by = 2);
	for(loc in 1:nloci) {
		gendat[,loc] = c(paste(Demedat[,allpos[loc]], Demedat[,(allpos[loc]+1)], sep = ""));
	}
  
	ID = paste0(c(runif(sample.size)))
	genepop.filler = rep(",", sample.size)	

	indat = cbind(ID, genepop.filler, gendat);
	indat;
}


temporal.input.wild = function(samp, nloci=8, temporal.file = NULL) {
	line1 = "Temporal samples before and after stocking (yrs: 0, 1, 45, 90)"
	line2 = paste("loc", c(1:nloci), sep = "");
	popsep = "POP";
	
	cat(line1, file = temporal.file, sep = "\n");
	cat(line2, file = temporal.file, sep = "\n", append = TRUE);
	cat(popsep, file = temporal.file, sep = "\n", append = TRUE);
	write.table(samp, file = temporal.file, append = TRUE, sep = " ", col.names = FALSE, row.names = FALSE, quote = FALSE);
}

temporal.input = function(samp, nloci=8, temporal.file = NULL) {
	popsep = "POP";
	cat(popsep, file = temporal.file, sep = "\n", append = TRUE);
	write.table(samp, file = temporal.file, append = TRUE, sep = " ", col.names = FALSE, row.names = FALSE, quote = FALSE);
}


LD.input.wild = function(samp, nloci=8, LD.file = NULL) {
	line1 = "LD samples throughout simulation"
	line2 = paste("loc", c(1:nloci), sep = "");
	popsep = "POP";
	
	cat(line1, file = LD.file, sep = "\n");
	cat(line2, file = LD.file, sep = "\n", append = TRUE);
	cat(popsep, file = LD.file, sep = "\n", append = TRUE);
	write.table(samp, file = LD.file, append = TRUE, sep = " ", col.names = FALSE, row.names = FALSE, quote = FALSE);
}

LD.input = function(samp, nloci=8, LD.file = NULL) {
	popsep = "POP";
	cat(popsep, file = LD.file, sep = "\n", append = TRUE);
	write.table(samp, file = LD.file, append = TRUE, sep = " ", col.names = FALSE, row.names = FALSE, quote = FALSE);
}

