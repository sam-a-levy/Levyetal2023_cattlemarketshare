#add the numbers
totdef_w <- 1.284e+13  # total deforestation weighted by area, post 2009
totdef_r <- 4740633.6  # total deforestation observed, post 2009
g4coef <-  -.7206433 
taccoef <- .256074
g4lowl <- -.3965039
taclowl <- .4084147
# also weighted by area, post 2009
mnG4  <-  0.2960209 
mnZDC <-  0.5232753
mnTAC <-  0.2272544

g4conint <-  (g4coef -   g4lowl) #taken from 95 conf interval
tacconint <- -1*(taccoef -  taclowl)

# total impact G4 = -8669.295 +- 4172.21km (18+-9%)
G4ef <- (expm1(g4coef)*mnG4) #0.1828658 effect
(G4ef*totdef_r)/100  #-8669.295 km2
#confidence interval G4
G4efconint <- (expm1(g4conint)*mnG4) #+-0.08800955 (
(G4efconint*totdef_r)/100 #-4172.21km

# total impact TAC = -8669.295 +- 4172.21km (18+-9%)
TACef <- (expm1(taccoef)*mnG4) #0.1828658 effect
(TACef*totdef_r)/100  #-8669.295 km2
#confidence interval TAC
TACefconint <- (expm1(tacconint)*mnG4) #+-0.08800955 (
(TACefconint*totdef_r)/100 #-4172.21k

#summed impact
G4ef+TACef
G4efconint+TACefconint
((G4ef*totdef_r)+(TACef*totdef_r))/100
((G4efconint*totdef_r)+(TACefconint*totdef_r))/100

# total impact G4 = -15324.69 +- 7375.204km2 (32+-16%) NOTE: 44% bigger
ZDCef <- (expm1(g4coef)*mnZDC) #0.3232624 effect
ZDCef
(ZDCef*totdef_r)/100  #-15324.69 km2
#confidence interval G4
ZDCefconint <- (expm1(g4conint)*mnZDC) #+--0.1555742
ZDCefconint
(ZDCefconint*totdef_r)/100 #-7375.204km2

#total impact if G4 had been 1
maxef <- (expm1(g4coef)*1) #0.3232624 effect
maxef
(maxef*totdef_r)/100  #-15324.69 km2
#confidence interval G4
maxefconint <- (expm1(g4conint)*1) #+--0.1555742
maxefconint
(maxefconint*totdef_r)/100 #-7375.20
