run.replicate = function()
{
    simrand = runif(1)

    #Burn-in phase
    burn.in = RDlandsweep() 
    	
    #Sweepstakes - 90 year burn.in  #10 generations
    for (n in 1:90)
    {
    burn.in = Sweepstakes(Rland=burn.in, Rland.new=burn.in)
    }
    
save(file=paste0("Replicate",simrand,".rda"), burn.in)
    
    samp.wild = landscape.sample(burn.in, ns = 500, pvec=3)
    samp.wild.Nb = landscape.sample(burn.in, ns = 250, svec=c(1,6))
    samp.wild.Ne = landscape.sample(burn.in, ns = 500, pvec=3)
    
#Diversity stats (burn.in)
    Wildstats = Divstats(samp.wild)
    Nb.wild = get.sample(samp.wild.Nb, sample.size=length(samp.wild.Nb$individuals[,1]))
    Ne.wild = get.sample(samp.wild.Ne, sample.size=length(samp.wild.Ne$individuals[,1]))
    Compile.wild(diversity = Wildstats, NbLD = Nb.wild, NeLD = Ne.wild, sim=simrand)

#Stocking year 1
    num.fem.brood = 1
    num.male.brood = 1
    
    fem.brood = vector("list", num.fem.brood)
    male.brood = vector("list", num.male.brood)
                        
    for(x in 1:num.fem.brood) { 
        fem.brood[[x]] = sample(which(burn.in$individuals[,1] == 28), 1, replace = FALSE);
        burn.in$individuals[fem.brood[[x]],1] = 39;          
    }
    
    for (y in 1:num.male.brood) {
        male.brood[[y]] = sample(which(burn.in$individuals[,1] == 23), 1, replace = FALSE);
        burn.in$individuals[male.brood[[y]],1] = 34
    }   
    
    num.stocked = 0.3*15000 #30% contribution to Age 1 fish
    per.cap.sweep.wild = 4
    per.cap.sweep.hat = 4
    burn.in = RDlandsweep(num.stocked=num.stocked, per.cap.sweep.wild=per.cap.sweep.wild,  per.cap.sweep.hat=per.cap.sweep.hat, rland=burn.in)   
    stock = Sweepstakes(Rland=burn.in, Rland.new=stock)
    
save(file=paste0("Replicate",simrand,".rda"), stock)
    
    samp.stock = landscape.sample(stock, ns = 500, pvec=3)   
    samp.Nb = landscape.sample(stock, ns = 250, svec=c(1,6)) 
    samp.Ne = landscape.sample(stock, ns = 500, pvec=3)
    
#Diversity stats
    stock.stats = Divstats(samp.stock)
    Nb.stock = get.sample(samp.Nb, sample.size=length(samp.Nb$individuals[,1]))
    Ne.stock = get.sample(samp.Ne, sample.size=length(samp.Ne$individuals[,1]))
    Compile(diversity = stock.stats, NbLD = Nb.stock, NeLD = Ne.stock, sim=simrand)

#Stocking period    #Stocking for 5 generations
  for (n in 1:44)
  {
  	num.fem.brood = 1
    num.male.brood = 1
    
    fem.brood = vector("list", num.fem.brood)
    male.brood = vector("list", num.male.brood)
                        
    for(x in 1:num.fem.brood) { 
        fem.brood[[x]] = sample(which(stock$individuals[,1] == 28), 1, replace = FALSE);
        stock$individuals[fem.brood[[x]],1] = 39;          
    }
    
    for (y in 1:num.male.brood) {
        male.brood[[y]] = sample(which(stock$individuals[,1] == 23), 1, replace = FALSE);
        stock$individuals[male.brood[[y]],1] = 34
    }   
    
    stock = RDlandsweep(num.stocked=num.stocked, per.cap.sweep.wild=per.cap.sweep.wild,  per.cap.sweep.hat=per.cap.sweep.hat, rland=stock)   
    stock = Sweepstakes(Rland=stock, Rland.new=stock)

save(file=paste0("Replicate",simrand,".rda"), stock)

    samp.stock = landscape.sample(stock, ns = 500, pvec=3)   
    samp.Nb = landscape.sample(stock, ns = 250, svec=c(1,6)) 
    samp.Ne = landscape.sample(stock, ns = 500, pvec=3)
    
#Diversity stats
    stock.stats = Divstats(samp.stock)
    Nb.stock = get.sample(samp.Nb, sample.size=length(samp.Nb$individuals[,1]))
    Ne.stock = get.sample(samp.Ne, sample.size=length(samp.Ne$individuals[,1]))
    Compile(diversity = stock.stats, NbLD = Nb.stock, NeLD = Ne.stock, sim=simrand)
    }

  	
#No more stocking for 45 years to see lagging effects.   #5 generation recovery
    per.cap.sweep.wild = 6
    per.cap.sweep.hat = 6
    stock = RDlandsweep(num.stocked=0, per.cap.sweep.wild=per.cap.sweep.wild,  per.cap.sweep.hat=per.cap.sweep.hat, rland=stock)
    recover = Sweepstakes(Rland=stock, Rland.new=recover)
    
save(file=paste0("Replicate",simrand,".rda"), recover)
    
    samp.recover = landscape.sample(recover, ns = 500, pvec=3)
    samp.recover.Nb = landscape.sample(recover, ns = 250, svec=c(1,6)) 
    samp.recover.Ne = landscape.sample(recover, ns = 500, pvec=3)
    
    Recover = Divstats(samp.recover) 
    Nb.recover = get.sample(samp.recover.Nb, sample.size=length(samp.recover.Nb$individuals[,1]))
    Ne.recover = get.sample(samp.recover.Ne, sample.size=length(samp.recover.Ne$individuals[,1]))
    Compile(diversity = Recover, NbLD = Nb.recover, NeLD = Ne.recover, sim=simrand)
    
    for (n in 1:44)
    {
        recover = RDlandsweep(num.stocked=0, per.cap.sweep.wild=per.cap.sweep.wild, per.cap.sweep.hat=per.cap.sweep.hat, rland=recover)
        recover = Sweepstakes(Rland=recover, Rland.new=recover)
        
save(file=paste0("Replicate",simrand,".rda"), recover)
        
        samp.recover = landscape.sample(recover, ns = 500, pvec=3)
        samp.recover.Nb = landscape.sample(recover, ns = 250, svec=c(1,6)) 
    	samp.recover.Ne = landscape.sample(recover, ns = 500, pvec=3)
    	
    	Recover = Divstats(samp.recover) 
    	Nb.recover = get.sample(samp.recover.Nb, sample.size=length(samp.recover.Nb$individuals[,1]))
    	Ne.recover = get.sample(samp.recover.Ne, sample.size=length(samp.recover.Ne$individuals[,1]))
    	Compile(diversity = Recover, NbLD = Nb.recover, NeLD = Ne.recover, sim=simrand)
    }

unlink(paste0("Replicate",simrand,".rda"))

recover 
}