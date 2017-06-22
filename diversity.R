#
#
#
#
Nei.Fstats <- function(Rland)
{   
    l1 = landscape.states(lnum=1, Rland)
    Loc1 = paste0(l1[,7], l1[,8], sep="")
    Loc1 = as.factor(Loc1)
    
    l2 = landscape.states(lnum=2, Rland)
    Loc2 = paste0(l2[,7], l2[,8], sep="")
    Loc2 = as.factor(Loc2)
    
    l3 = landscape.states(lnum=3, Rland)
    Loc3 = paste0(l3[,7], l3[,8], sep="")
    Loc3 = as.factor(Loc3)
    
    l4 = landscape.states(lnum=4, Rland)
    Loc4 = paste0(l4[,7], l4[,8], sep="")
    Loc4 = as.factor(Loc4)
    
    l5 = landscape.states(lnum=5, Rland)
    Loc5 = paste0(l5[,7], l5[,8], sep="")
    Loc5 = as.factor(Loc5)
    
    l6 = landscape.states(lnum=6, Rland)
    Loc6 = paste0(l6[,7], l6[,8], sep="") 
    Loc6 = as.factor(Loc6)
    
    l7 = landscape.states(lnum=7, Rland)
    Loc7 = paste0(l7[,7], l7[,8], sep="")
    Loc7 = as.factor(Loc7)
    
    l8 = landscape.states(lnum=8, Rland)
    Loc8 = paste0(l8[,7], l8[,8], sep="")
    Loc8 = as.factor(Loc8)
    
    Wild = (length(which(Rland$individuals[,1] == 3))) + (length(which(Rland$individuals[,1] == 8)))
    W = rep(1, Wild)
    Cultured = (length(which(Rland$individuals[,1] == 12))) + (length(which(Rland$individuals[,1] == 17)))
    C = rep(2, Cultured)
    population = as.factor(c(W, C))
    
    combine = cbind(population, Loc1, Loc2, Loc3, Loc4, Loc5, Loc6, Loc7, Loc8)
    stat = basic.stats(combine, diploid=T)
    stat
}

Allele.numb <- function(Rland)
{   
    count = landscape.allelecount(Rland)
    ac = count[count$pop==3,]
    Loc1 = sum(ac[,4]==1)
    Loc2 = sum(ac[,4]==2)
    Loc3 = sum(ac[,4]==3)
    Loc4 = sum(ac[,4]==4)
    Loc5 = sum(ac[,4]==5)
    Loc6 = sum(ac[,4]==6)
    Loc7 = sum(ac[,4]==7)
    Loc8 = sum(ac[,4]==8)
    allele = cbind(Loc1,Loc2,Loc3,Loc4,Loc5,Loc6,Loc7,Loc8)
    allele
}

Allele.rare <- function(Rland)
{   
    count = landscape.allelecount(Rland)
    ac = count[count$pop==3,]
    
    Loc1 = ac[ac$loc==1,2]
    one = sum(Loc1)
    Loc1 = ac[ac$loc==1,2]<(0.01*one)
    Loc1 = sum(Loc1)
    
    Loc2 = ac[ac$loc==2,2]
    two = sum(Loc2)
    Loc2 = ac[ac$loc==2,2]<(0.01*two)
    Loc2 = sum(Loc2)
    
    Loc3 = ac[ac$loc==3,2]
    three = sum(Loc3)
    Loc3 = ac[ac$loc==3,2]<(0.01*three)
	Loc3 = sum(Loc3)
	
	Loc4 = ac[ac$loc==4,2]
    four = sum(Loc4)
    Loc4 = ac[ac$loc==4,2]<(0.01*four)
	Loc4 = sum(Loc4)
	
	Loc5 = ac[ac$loc==5,2]
    five = sum(Loc5)
    Loc5 = ac[ac$loc==5,2]<(0.01*five)
	Loc5 = sum(Loc5)
	
	Loc6 = ac[ac$loc==6,2]
    six = sum(Loc6)
    Loc6 = ac[ac$loc==6,2]<(0.01*six)
	Loc6 = sum(Loc6)
	
	Loc7 = ac[ac$loc==7,2]
    seven = sum(Loc7)
    Loc7 = ac[ac$loc==7,2]<(0.01*seven)
	Loc7 = sum(Loc7)
	
	Loc8 = ac[ac$loc==8,2]
    eight = sum(Loc8)
    Loc8 = ac[ac$loc==8,2]<(0.01*eight)
	Loc8 = sum(Loc8)
	
    allele = cbind(Loc1,Loc2,Loc3,Loc4,Loc5,Loc6,Loc7,Loc8)
    allele
}


A.range = function(Rland)
{   
    l1 = landscape.states(lnum=1, Rland)
    one = c(l1[,7],l1[,8])
    Loc1 = data.frame(range(one))
    Loc1 = Loc1[2,1]-Loc1[1,1]

    l2 = landscape.states(lnum=2, Rland)
    two = c(l2[,7],l2[,8])
    Loc2 = data.frame(range(two))
    Loc2 = Loc2[2,1]-Loc2[1,1]
    
    l3 = landscape.states(lnum=3, Rland)
    three = c(l3[,7],l3[,8])
    Loc3 = data.frame(range(three))
    Loc3 = Loc3[2,1]-Loc3[1,1]
    
    l4 = landscape.states(lnum=4, Rland)
    four = c(l4[,7],l4[,8])
    Loc4 = data.frame(range(four))
    Loc4 = Loc4[2,1]-Loc4[1,1]
    
    l5 = landscape.states(lnum=5, Rland)
    five = c(l5[,7],l5[,8])
    Loc5 = data.frame(range(five))
    Loc5 = Loc5[2,1]-Loc5[1,1]
    
    l6 = landscape.states(lnum=6, Rland)
    six = c(l6[,7],l6[,8])
    Loc6 = data.frame(range(six))
    Loc6 = Loc6[2,1]-Loc6[1,1]
    
    l7 = landscape.states(lnum=7, Rland)
    seven = c(l7[,7],l7[,8])
    Loc7 = data.frame(range(seven))
    Loc7 = Loc7[2,1]-Loc7[1,1]
    
    l8 = landscape.states(lnum=8, Rland)
    eight = c(l8[,7],l8[,8])
    Loc8 = data.frame(range(eight))
    Loc8 = Loc8[2,1]-Loc8[1,1]
    
    range = cbind(Loc1,Loc2,Loc3,Loc4,Loc5,Loc6,Loc7,Loc8)
    range
}


landscape.effective.alleles <- function(Rland)  
{
    hom <- 1-landscape.exp.het(Rland)
    1/hom 
}

#He
landscape.exp.het <- function(Rland)
  {
    tot <- 0
    pops <- unique(landscape.populations(Rland))
    rl <- matrix(NA,nrow=Rland$intparam$habitats,ncol=length(Rland$loci))
    for (j in (pops))
      {
        for (loc in 1:length(Rland$loci))
          {
            tab <- table(landscape.locus(loc,Rland)[landscape.populations(Rland)==j,c(-1:-(landscape.democol()))])
            sctab <- tab/sum(tab)
            rl[j,loc] <- 1 - sum(sctab^2)
          }
      }
    rl
  }

#He
#landscape.exp.het.old <- function(Rland)
#  {
#    tot <- 0
#    rl <- matrix(0,nrow=Rland$intparam$habitats,ncol=length(Rland$loci))
#    for (j in 1:length(unique(landscape.populations(Rland))))
#      {
#        for (loc in 1:length(Rland$loci))
#          {
#            tab <- table(landscape.locus(loc,Rland)[landscape.populations(Rland)==j,c(-1:-(landscape.democol()))])
#            sctab <- tab/sum(tab)
#            rl[j,loc] <- 1 - sum(sctab^2)
#          }
#      }
#    rl
#  }

#Ho
landscape.obs.het <- function(Rland)
  {
    tot <- 0
    rl <- matrix(NA,nrow=Rland$intparam$habitats,ncol=length(Rland$loci))
    for (j in unique(landscape.populations(Rland)))
      {
        for (loc in 1:length(Rland$loci))
          {
            if (landscape.ploidy(Rland)[loc]==1) #obs het doesn't make sense for a haploid locus
              {
                rl[j,loc] <- NA
              }  else {
                freq.df <- data.frame(table(landscape.locus(loc,Rland)[landscape.populations(Rland)==j,c((-1:-landscape.democol()),-8)],landscape.locus(loc,Rland)[landscape.populations(Rland)==j,c((-1:-landscape.democol()),-7)]))
                rl[j,loc] <- (1-sum(freq.df[as.character(freq.df[,1])==as.character(freq.df[,2]),3])/sum(freq.df[,3]))
              }
          }
      }
    rl
  }


landscape.FWright <- function (Rland)
  {
    1-landscape.obs.het(Rland)/landscape.exp.het(Rland)
  }


landscape.allelefreq.old <- function(Rland,tbl.out=FALSE)
  {
    rv <- NULL
    for (i in 1:length(Rland$loci))
      {
        pops <- vector("list",length(Rland$loci))
        for (j in unique(landscape.populations(Rland)))
          {
            alleles     <- landscape.locus(i,Rland)[landscape.populations(Rland)==j,c(-1:-(landscape.democol()))]
            freqtbl     <- table(alleles)
            scframe     <- data.frame(freqtbl/sum(freqtbl))
            scframe$pop <- rep(j,dim(scframe)[1])
            scframe$loc <- rep(i,dim(scframe)[1])
            rv <- rbind(rv,scframe)
          }
      }
    rownames(rv) <- 1:dim(rv)[1]
    if (tbl.out==TRUE)
      {
        xtabs(Freq~pop+alleles+loc,rv)
      } else {
      rv
    }
  }

landscape.allelecount <- function(Rland,tbl.out=FALSE)
  {
    rv <- NULL
    for (i in 1:length(Rland$loci))
      {
        pops <- vector("list",length(Rland$loci))
        for (j in unique(landscape.populations(Rland)))
          {
            alleles     <- landscape.locus(i,Rland)[landscape.populations(Rland)==j,c(-1:-(landscape.democol()))]
            freqtbl     <- table(alleles)
            scframe     <- data.frame(freqtbl)
            scframe$pop <- rep(j,dim(scframe)[1])
            scframe$loc <- rep(i,dim(scframe)[1])
            rv <- rbind(rv,scframe)
          }
      }
    rownames(rv) <- 1:dim(rv)[1]
    if (tbl.out==TRUE)
      {
        xtabs(Freq~pop+alleles+loc,rv)
      } else {
      rv
    }
  }
