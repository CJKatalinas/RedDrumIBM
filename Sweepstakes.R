Sweepstakes = function(Rland, Rland.new, randsweep, Stocked.adults.male, Stocked.adults.fem)
{
Stocked.adults.male <- (length(which(Rland$individuals[,1]==24)))
Stocked.adults.fem <- (length(which(Rland$individuals[,1]==29)))

if (Stocked.adults.male < 1 | Stocked.adults.fem < 1) 
{

randsweep = 0.10
fem.sweep.wild = randsweep*(length(which(Rland$individuals[,1]==28)))
male.sweep.wild = randsweep*(length(which(Rland$individuals[,1]==23)))

fem.win.wild = vector("list", fem.sweep.wild)
male.win.wild = vector("list", male.sweep.wild)

for(x in 1:fem.sweep.wild) { 
    fem.win.wild[[x]] = sample(which(Rland$individuals[,1] == 28), 1, replace = FALSE);
    Rland$individuals[fem.win.wild[[x]],1] = 48;          
}

for (y in 1:male.sweep.wild) {
    male.win.wild[[y]] = sample(which(Rland$individuals[,1] == 23), 1, replace = FALSE);
    Rland$individuals[male.win.wild[[y]],1] = 43
}    

write(table(Rland$individuals[,1]), file="Population size-sweep.txt", ncolumns=50, append=T, sep = "\t")

Rland.new = landscape.simulate(Rland, 1)

} else {

    randsweep = 0.10
    fem.sweep.wild = randsweep*(length(which(Rland$individuals[,1]==28)))
    male.sweep.wild = randsweep*(length(which(Rland$individuals[,1]==23)))
    
    fem.win.wild = vector("list", fem.sweep.wild)
    male.win.wild = vector("list", male.sweep.wild)
    
    for(x in 1:fem.sweep.wild) { 
        fem.win.wild[[x]] = sample(which(Rland$individuals[,1] == 28), 1, replace = FALSE);
        Rland$individuals[fem.win.wild[[x]],1] = 48;          
    }
    
    for (y in 1:male.sweep.wild) {
        male.win.wild[[y]] = sample(which(Rland$individuals[,1] == 23), 1, replace = FALSE);
        Rland$individuals[male.win.wild[[y]],1] = 43
    }      
     
    fem.sweep.hat = randsweep*(length(which(Rland$individuals[,1]==29)))
    male.sweep.hat = randsweep*(length(which(Rland$individuals[,1]==24)))  
    
    fem.win.hat = vector("list", fem.sweep.hat)
    male.win.hat = vector("list", male.sweep.hat)
    
    for(x in 1:fem.sweep.hat) { 
        fem.win.hat[[x]] = sample(which(Rland$individuals[,1] == 29), 1, replace = FALSE);
        Rland$individuals[fem.win.hat[[x]],1] = 49;          
    }
    
    for (y in 1:male.sweep.hat) {
        male.win.hat[[y]] = sample(which(Rland$individuals[,1] == 24), 1, replace = FALSE);
        Rland$individuals[male.win.hat[[y]],1] = 44
    }      
    
write(table(Rland$individuals[,1]), file="Population size-sweep.txt", ncolumns=50, append=T, sep = "\t")
    
    Rland.new = landscape.simulate(Rland, 1)
 }
}

