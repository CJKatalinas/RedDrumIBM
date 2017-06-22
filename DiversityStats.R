Divstats = function(rland, Stats=list(He=Exp.Het, He.mean=mean.He, He.harmean=harmean.He, Ho=Obs.Het, Ho.mean=mean.Ho, Ho.harmean=harmean.Ho, Allelenumb=allele.numb, Allelenumb.mean=mean.anumb, Allelenumb.harmean=harmean.anumb, AlleleRare=allele.rare, AlleleRare.mean=mean.rare, AlleleRare.harmean=harmean.rare, EffectiveAlleles=Effective.alleles, EffectiveAlleles.mean=mean.Effective, EffectiveAlleles.harmean=harmean.Effective, Allelerange=allele.range, Allelerange.mean=mean.range, Allelerange.harmean=harmean.range, Fis=Fis, Fis.mean=mean.Fis))
{ {
    Exp.Het = landscape.exp.het(rland)
    Exp.Het = Exp.Het[3,]
    mean.He = mean(Exp.Het)
    harmean.He = harmonic.mean(Exp.Het)
    
    Obs.Het = landscape.obs.het(rland)
    Obs.Het = Obs.Het[3,]
    mean.Ho = mean(Obs.Het)
    harmean.Ho = harmonic.mean(Obs.Het)
    
    allele.numb = Allele.numb(rland)
    mean.anumb = mean(allele.numb[1,])
    harmean.anumb = harmonic.mean(allele.numb[1,])
    
    allele.rare = Allele.rare(rland)
    mean.rare = mean(allele.rare[1,])
    harmean.rare = harmonic.mean(allele.rare[1,])
    
    Effective.alleles = landscape.effective.alleles(rland)
    Effective.alleles = Effective.alleles[3,]
    mean.Effective = mean(Effective.alleles)
    harmean.Effective = harmonic.mean(Effective.alleles)
    
    allele.range = A.range(rland)
    mean.range = mean(allele.range[1,])
    harmean.range = harmonic.mean(allele.range[1,])
    
    Fis = landscape.FWright(rland)
    Fis = Fis[3,]
    mean.Fis = mean(Fis)
}
Stats
}


