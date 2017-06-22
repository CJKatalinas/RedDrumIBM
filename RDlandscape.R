RDlandsweep = function (
    surv.Age2.wild = 0.69,  #Proportion of fish that survive from Age 1 to 2.
    surv.Age2.hat = 0.69,
    
    surv.Age3.wild = 0.78,
    surv.Age3.hat = 0.78,
    
    surv.Age4.wild = 0.84, 
    surv.Age4.hat = 0.84,
    
    surv.Age5.wild = 0.86,     
    surv.Age5.hat = 0.86,
    
    surv.Adult.wild = 0.89,
    surv.Adult.hat = 0.89,
    sweep.surv = 1,   
    k = 500000,  
    
    num.male.brood = length(which(rland$individuals[,1]==34)),
    num.fem.brood = length(which(rland$individuals[,1]==39)),  
    per.cap.sweep.wild = 6,
    per.cap.sweep.hat = 6,
    Repr.sweep.wild = (per.cap.sweep.wild/2),
    Repr.sweep.hat = (per.cap.sweep.hat/2),
    num.stocked = 0,
    Male.stocked = num.stocked/(2*num.fem.brood),
    Fem.stocked = num.stocked/(2*num.fem.brood),
    #male.contr = 1,
    loci = 8,
    rland = NULL)
        
{
    if (is.null(rland))
    {
    rland <- landscape.new.empty()
    rland <- landscape.new.intparam(rland, h = 5, s = 10)
    rland <- landscape.new.switchparam(rland, mp = 1)  
    rland <- landscape.new.floatparam(rland)
  
    S <- matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                  surv.Age2.wild, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                  0, surv.Age3.wild, 0, 0, 0, 0, 0, 0, 0, 0,
                  0, 0, surv.Age4.wild, 0, 0, 0, 0, 0, 0, 0,
                  0, 0, 0, surv.Age5.wild, 0, 0, 0, 0, 0, 0,
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                  0, 0, 0, 0, 0, surv.Age2.wild, 0, 0, 0, 0,
                  0, 0, 0, 0, 0, 0, surv.Age3.wild, 0, 0, 0,
                  0, 0, 0, 0, 0, 0, 0, surv.Age4.wild, 0, 0,
                  0, 0, 0, 0, 0, 0, 0, 0, surv.Age5.wild, 0), byrow = TRUE, nrow = 10)
    
        R <-  matrix(rep(0, 10*10), byrow = TRUE, nrow = 10)
    
        M <-  matrix(rep(0, 10*10), byrow = TRUE, nrow = 10) 
  
    rland <- landscape.new.local.demo(rland, S, R, M) #Wild Sub-adult pop'n
  
    S <- matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                  surv.Age2.hat, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                  0, surv.Age3.hat, 0, 0, 0, 0, 0, 0, 0, 0,
                  0, 0, surv.Age4.hat, 0, 0, 0, 0, 0, 0, 0,
                  0, 0, 0, surv.Age5.hat, 0, 0, 0, 0, 0, 0,
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                  0, 0, 0, 0, 0, surv.Age2.hat, 0, 0, 0, 0,
                  0, 0, 0, 0, 0, 0, surv.Age3.hat, 0, 0, 0,
                  0, 0, 0, 0, 0, 0, 0, surv.Age4.hat, 0, 0,
                  0, 0, 0, 0, 0, 0, 0, 0, surv.Age5.hat, 0), byrow = TRUE, nrow = 10)
    
        R <-  matrix(rep(0, 10*10), byrow = TRUE, nrow = 10)
    
        M <-  matrix(rep(0, 10*10), byrow = TRUE, nrow = 10)
  
    rland <- landscape.new.local.demo(rland, S, R, M) #Cultured Sub-adult pop'n
  
        S <- matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, surv.Adult.wild, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, surv.Adult.hat, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, surv.Adult.wild, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, surv.Adult.hat), byrow = TRUE, nrow = 10)
  
        R <-  matrix(rep(0, 10*10), byrow = TRUE, nrow = 10)
  
        M <-  matrix(rep(0, 10*10), byrow = TRUE, nrow = 10)
  
    rland <- landscape.new.local.demo(rland, S, R, M) #Adult pop'n
    
        S <-  matrix(rep(0, 10*10), byrow = TRUE, nrow = 10)
    
        R <-  matrix(rep(0, 10*10), byrow = TRUE, nrow = 10)
    
        M <-  matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                   0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                   0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                   0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                   0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                   0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                   0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                   0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                   0, 0, 0, 0, 1, 0, 0, 0, 0, 0), byrow = TRUE, nrow = 10)
    
    rland <- landscape.new.local.demo(rland, S, R, M) #Hatchery broodstock
        
        S <-  matrix(rep(0, 10*10), byrow = TRUE, nrow = 10)
    
        R <-  matrix(rep(0, 10*10), byrow = TRUE, nrow = 10)
    
        M <-  matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                       0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                       0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                       0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                       0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                       0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                       0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                       0, 0, 0, 0.5, 0.5, 0, 0, 0, 0, 0,
                       0, 0, 0, 0.5, 0.5, 0, 0, 0, 0, 0), byrow = TRUE, nrow = 10)
    
    rland <- landscape.new.local.demo(rland, S, R, M) #Contributing Adults
  
        S <- matrix(rep(0, 50*50), byrow = TRUE, nrow = 50)
        R <- S
        M <- S
            
        S[24,5] = 1
        S[29,10] = 1
        S[25,15] = 1
        S[30,20] = 1
        S[24,35] = 1
        S[29,40] = 1
        S[24,44] = sweep.surv
        S[25,45] = sweep.surv
        S[29,49] = sweep.surv
        S[30,50] = sweep.surv
    
        R[1,49] = Repr.sweep.wild
        R[6,49] = Repr.sweep.wild
        R[1,50] = Repr.sweep.hat
        R[6,50] = Repr.sweep.hat
  
        R[11,40] = Male.stocked
        R[16,40] = Fem.stocked
     
  
    rland <- landscape.new.epoch(rland, S = S, R = R, M = M, carry = c(100000, 90000, 50000, 10, 10000))
  
    rland <- landscape.new.locus(rland, type = 1, ploidy = 2, mutationrate = 0.0001, transmission = 0, numalleles = 16, frequencies = c(.0054, .0416, .0108, .0090, .0009, .0027, .2523, .0344, .0027, .2595, .1926, .0832, .0163, .0651, .0127, .0108), states = c(110, 114, 116, 118, 120, 122, 124, 126, 128, 130, 132, 134, 136, 138, 140, 142))
    rland <- landscape.new.locus(rland, type = 1, ploidy = 2, mutationrate = 0.0001, transmission = 0, numalleles = 14, frequencies = c(.0064, .0045, .0036, .0136, .0082, .1851, .1751, .1951, .1452, .1071, .1261, .0091, .0118, .0091), states = c(106, 108, 112, 114, 116, 118, 120, 122, 124, 126, 128, 130, 132, 134))
    rland <- landscape.new.locus(rland, type = 1, ploidy = 2, mutationrate = 0.0001, transmission = 0, numalleles = 5, frequencies = c(.2324, .0252, .1829, .5360, .0234), states = c(127, 129, 131, 133, 135))
    rland <- landscape.new.locus(rland, type = 1, ploidy = 2, mutationrate = 0.0001, transmission = 0, numalleles = 5, frequencies = c(.0683, .0470, .4308, .4336, .0203), states = c(154, 157, 160, 163, 166))
    rland <- landscape.new.locus(rland, type = 1, ploidy = 2, mutationrate = 0.0001, transmission = 0, numalleles = 20, frequencies = c(.0027, .0127, .0344, .0271, .0696, .0769, .0904, .1193, .1221, .1193, .1094, .0832, .0570, .0298, .0190, .0054, .0127, .0027, .0045, .0018), states = c(111, 115, 119, 123, 127, 131, 135, 139, 143, 147, 151, 155, 159, 163, 167, 171, 175, 179, 187, 199))
    rland <- landscape.new.locus(rland, type = 1, ploidy = 2, mutationrate = 0.0001, transmission = 0, numalleles = 6, frequencies = c(.0102, .4239, .1698, .2458, .1419, .0083), states = c(188, 191, 194, 197, 200, 206))
    rland <- landscape.new.locus(rland, type = 1, ploidy = 2, mutationrate = 0.0001, transmission = 0, numalleles = 17, frequencies = c(.0144, .0144, .1462, .2356, .0722, .1498, .0325, .0379, .0253, .0496, .1029, .0388, .0487, .0081, .0162, .0009, .0063), states = c(118, 120, 124, 126, 128, 130, 132, 134, 136, 138, 140, 142, 144, 146, 148, 150, 156))
    rland <- landscape.new.locus(rland, type = 1, ploidy = 2, mutationrate = 0.0001, transmission = 0, numalleles = 11, frequencies = c(.0009, .0137, .0009, .5619, .0082, .0091, .3151, .0027, .0301, .0565, .0009), states = c(117, 119, 121, 123, 127, 129, 131, 133, 135, 141, 155))

    rland <- landscape.new.individuals(rland, c(7500, 5186, 4058, 3393, 2919, 7500, 5186, 4058, 3393, 2919, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 25000, 0, 0, 0, 0, 25000, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
    } else{
     
        R = rland$demography$epochs[[1]]$R
        R[1,49] = Repr.sweep.wild
        R[6,49] = Repr.sweep.wild
        R[1,50] = Repr.sweep.hat
        R[6,50] = Repr.sweep.hat
        
        R[11,40] = Male.stocked
        R[16,40] = Fem.stocked
        rland$demography$epochs[[1]]$R = R
        
    }
  rland  
}


