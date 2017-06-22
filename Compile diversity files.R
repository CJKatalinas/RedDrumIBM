Compile.wild = function(diversity, NbLD, NeLD, sim) {
  
  Dir = getwd()
  {
  setwd("/Users/christophermealey/Desktop/Results/He")
  write(diversity$He, file=paste0("Exp-Het",sim,".txt"), ncolumns= 8, append=T, sep = "\t")
  
  setwd("/Users/christophermealey/Desktop/Results/He_mean")
  write(diversity$He.mean, file=paste0("mean_Exp-Het",sim,".txt"), ncolumns= 1, append=T, sep = "\t")
  
  setwd("/Users/christophermealey/Desktop/Results/He_harmonic")
  write(diversity$He.harmean, file=paste0("harmean_Exp-Het",sim,".txt"), ncolumns= 1, append=T, sep = "\t")
  
  setwd("/Users/christophermealey/Desktop/Results/Ho")
  write(diversity$Ho, file=paste0("Obs-Het",sim,".txt"), ncolumns= 8, append=T, sep = "\t")
  
  setwd("/Users/christophermealey/Desktop/Results/Ho_mean")
  write(diversity$Ho.mean, file=paste0("mean_Obs-Het",sim,".txt"), ncolumns= 1, append=T, sep = "\t")
  
  setwd("/Users/christophermealey/Desktop/Results/Ho_harmonic")
  write(diversity$Ho.harmean, file=paste0("harmean_Obs-Het",sim,".txt"), ncolumns= 1, append=T, sep = "\t")
  
  setwd("/Users/christophermealey/Desktop/Results/Allele count")
  write(diversity$Allelenumb, file=paste0("Allele-count",sim,".txt"), ncolumns=8, append=T, sep="\t")
  
  setwd("/Users/christophermealey/Desktop/Results/Allele count_mean")
  write(diversity$Allelenumb.mean, file=paste0("mean_Allele-count",sim,".txt"), ncolumns= 1, append=T, sep = "\t")
  
  setwd("/Users/christophermealey/Desktop/Results/Allele count_harmonic")
  write(diversity$Allelenumb.harmean, file=paste0("harmean_Allele-count",sim,".txt"), ncolumns= 1, append=T, sep = "\t")
  
  setwd("/Users/christophermealey/Desktop/Results/Allele rare")
  write(diversity$AlleleRare, file=paste0("Allele-rare",sim,".txt"), ncolumns= 8, append=T, sep = "\t")
  
  setwd("/Users/christophermealey/Desktop/Results/Allele rare_mean")
  write(diversity$AlleleRare.mean, file=paste0("mean_Allele-rare",sim,".txt"), ncolumns= 1, append=T, sep = "\t")

  setwd("/Users/christophermealey/Desktop/Results/Allele rare_harmonic")
  write(diversity$AlleleRare.harmean, file=paste0("harmean_Allele-rare",sim,".txt"), ncolumns= 1, append=T, sep = "\t")
  
  setwd("/Users/christophermealey/Desktop/Results/Effective alleles")
  write(diversity$EffectiveAlleles, file=paste0("Effect-allele",sim,".txt"), ncolumns= 8, append=T, sep = "\t")
  
  setwd("/Users/christophermealey/Desktop/Results/Effective alleles_mean")
  write(diversity$EffectiveAlleles.mean, file=paste0("mean_Effective-allele",sim,".txt"), ncolumns= 1, append=T, sep = "\t")
  
  setwd("/Users/christophermealey/Desktop/Results/Effective alleles_harmonic")
  write(diversity$EffectiveAlleles.harmean, file=paste0("harmean_Effective-allele",sim,".txt"), ncolumns= 1, append=T, sep = "\t")
  
  setwd("/Users/christophermealey/Desktop/Results/Allele range")
  write(diversity$Allelerange, file=paste0("Allelic-range",sim,".txt"), ncolumns= 8, append=T, sep = "\t")
  
  setwd("/Users/christophermealey/Desktop/Results/Allele range_mean")
  write(diversity$Allelerange.mean, file=paste0("mean_Allelic-range",sim,".txt"), ncolumns= 1, append=T, sep = "\t")
  
  setwd("/Users/christophermealey/Desktop/Results/Allele range_harmonic")
  write(diversity$Allelerange.harmean, file=paste0("harmean_Allelic-range",sim,".txt"), ncolumns= 1, append=T, sep = "\t")
  
  setwd("/Users/christophermealey/Desktop/Results/Nb")
  LD.input.wild(NbLD, LD.file=paste0("Nb",sim,".txt"))
  
  setwd("/Users/christophermealey/Desktop/Results/Ne")
  LD.input.wild(NeLD, LD.file=paste0("Ne",sim,".txt"))
  
  setwd("/Users/christophermealey/Desktop/Results/Fis")
  write(diversity$Fis, file=paste0("Fis",sim,".txt"), ncolumns=1, append=T, sep = "\t")
  
  setwd("/Users/christophermealey/Desktop/Results/Fis_mean")
  write(diversity$Fis.mean, file=paste0("mean_Fis",sim,".txt"), ncolumns=1, append=T, sep = "\t")
  }
  setwd(Dir)
}

Compile = function(diversity, NbLD, NeLD, NbTemporal, NeTemporal, sim) {
  
  Dir = getwd()
  {
  setwd("/Users/christophermealey/Desktop/Results/He")
  write(diversity$He, file=paste0("Exp-Het",sim,".txt"), ncolumns= 8, append=T, sep = "\t")
  
  setwd("/Users/christophermealey/Desktop/Results/He_mean")
  write(diversity$He.mean, file=paste0("mean_Exp-Het",sim,".txt"), ncolumns= 1, append=T, sep = "\t")
  
  setwd("/Users/christophermealey/Desktop/Results/He_harmonic")
  write(diversity$He.harmean, file=paste0("harmean_Exp-Het",sim,".txt"), ncolumns= 1, append=T, sep = "\t")
    
  setwd("/Users/christophermealey/Desktop/Results/Ho")
  write(diversity$Ho, file=paste0("Obs-Het",sim,".txt"), ncolumns= 8, append=T, sep = "\t")
  
  setwd("/Users/christophermealey/Desktop/Results/Ho_mean")
  write(diversity$Ho.mean, file=paste0("mean_Obs-Het",sim,".txt"), ncolumns= 1, append=T, sep = "\t")
  
  setwd("/Users/christophermealey/Desktop/Results/Ho_harmonic")
  write(diversity$Ho.harmean, file=paste0("harmean_Obs-Het",sim,".txt"), ncolumns= 1, append=T, sep = "\t")
  
  setwd("/Users/christophermealey/Desktop/Results/Allele count")
  write(diversity$Allelenumb, file=paste0("Allele-count",sim,".txt"), ncolumns=8, append=T, sep="\t")
  
  setwd("/Users/christophermealey/Desktop/Results/Allele count_mean")
  write(diversity$Allelenumb.mean, file=paste0("mean_Allele-count",sim,".txt"), ncolumns= 1, append=T, sep = "\t")
  
  setwd("/Users/christophermealey/Desktop/Results/Allele count_harmonic")
  write(diversity$Allelenumb.harmean, file=paste0("harmean_Allele-count",sim,".txt"), ncolumns= 1, append=T, sep = "\t")
  
  setwd("/Users/christophermealey/Desktop/Results/Allele rare")
  write(diversity$AlleleRare[1,], file=paste0("Allele-rare",sim,".txt"), ncolumns= 8, append=T, sep = "\t")
  
  setwd("/Users/christophermealey/Desktop/Results/Allele rare_mean")
  write(diversity$AlleleRare.mean, file=paste0("mean_Allele-rare",sim,".txt"), ncolumns= 1, append=T, sep = "\t")

  setwd("/Users/christophermealey/Desktop/Results/Allele rare_harmonic")
  write(diversity$AlleleRare.harmean, file=paste0("harmean_Allele-rare",sim,".txt"), ncolumns= 1, append=T, sep = "\t")
  
  setwd("/Users/christophermealey/Desktop/Results/Effective alleles")
  write(diversity$EffectiveAlleles, file=paste0("Effect-allele",sim,".txt"), ncolumns= 8, append=T, sep = "\t")
  
  setwd("/Users/christophermealey/Desktop/Results/Effective alleles_mean")
  write(diversity$EffectiveAlleles.mean, file=paste0("mean_Effective-allele",sim,".txt"), ncolumns= 1, append=T, sep = "\t")
  
  setwd("/Users/christophermealey/Desktop/Results/Effective alleles_harmonic")
  write(diversity$EffectiveAlleles.harmean, file=paste0("harmean_Effective-allele",sim,".txt"), ncolumns= 1, append=T, sep = "\t")
  
  setwd("/Users/christophermealey/Desktop/Results/Allele range")
  write(diversity$Allelerange, file=paste0("Allelic-range",sim,".txt"), ncolumns= 8, append=T, sep = "\t")
  
  setwd("/Users/christophermealey/Desktop/Results/Allele range_mean")
  write(diversity$Allelerange.mean, file=paste0("mean_Allelic-range",sim,".txt"), ncolumns= 1, append=T, sep = "\t")
  
  setwd("/Users/christophermealey/Desktop/Results/Allele range_harmonic")
  write(diversity$Allelerange.harmean, file=paste0("harmean_Allelic-range",sim,".txt"), ncolumns= 1, append=T, sep = "\t")
  
  setwd("/Users/christophermealey/Desktop/Results/Nb")
  LD.input(NbLD, LD.file=paste0("Nb",sim,".txt"))
  
  setwd("/Users/christophermealey/Desktop/Results/Ne")
  LD.input(NeLD, LD.file=paste0("Ne",sim,".txt"))
  
  setwd("/Users/christophermealey/Desktop/Results/Fis")
  write(diversity$Fis, file=paste0("Fis",sim,".txt"), ncolumns=1, append=T, sep = "\t")
  
  setwd("/Users/christophermealey/Desktop/Results/Fis_mean")
  write(diversity$Fis.mean, file=paste0("mean_Fis",sim,".txt"), ncolumns=1, append=T, sep = "\t")
  }
  setwd(Dir)
}

