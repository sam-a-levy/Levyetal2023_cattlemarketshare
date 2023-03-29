#' ---
#' Title: "Code to determine effect of G4 market share under range of scenarios"
#' Author: "SA Levy"
#' Date: 2023-03-21
#' ---

dat <- read_csv("/Users/sal9799/Downloads/rstudio-export/Levyetal2023_muni.csv")

#add the numbers
totdef_w <- sum(dat_r$defor*dat_r$muni_area)  # total deforestation weighted by area, post 2009 - 1.284458e+13
totdef_r <-  sum(dat_r$defor)               # total deforestation observed, post 2009 - 4740634
g4coef <-  -0.7206433                     # see table 1 (1) & S5 (1) + Levyetal2023_regressions_muni.do
taccoef <- 0.256074                       # see table 1 (1) & S5 (1) + Levyetal2023_regressions_muni.do
g4lowl <- -0.3965039                      # see Levyetal2023_regressions_muni.do - 95th conf interval (low)
taclowl <- 0.4084147                      # see Levyetal2023_regressions_muni.do - 95th conf interval (high)
# also weighted by area, post 2009
mnG4  <-  weighted.mean(dat_r$g4_ms,dat_r$muni_area)               # 0.2926364
mnZDC <-  weighted.mean((dat_r$g4_ms+dat_r$tac_ms),dat$muni_area)  # 0.5725956
mnTAC <-  weighted.mean(dat_r$tac_ms,dat_r$muni_area)              # 0.2799592

lm(dat_r$g4_ms ~ 1, weights = dat_r$muni_area)

mean(dat_r$g4_ms)

# mnG4  <-  0.2960209 
# mnZDC <-  0.5232753
# mnTAC <-  0.2272544

g4conint <-  (g4coef -   g4lowl) #taken from 95 conf interval
tacconint <- -1*(taccoef -  taclowl)

# total impact G4 = -7124.536 +- -3840.698
G4ef <- (expm1(g4coef)*mnG4) # -0.1502866 effect
(G4ef*totdef_r)/100  #-7124.536 km2
#confidence interval G4
G4efconint <- (expm1(g4conint)*mnG4) # -0.08101656
(G4efconint*totdef_r)/100 #-3840.698

# total impact G4 = -13940.43 km2 +- 7515.015
ZDCef <- (expm1(g4coef)*mnZDC) #-0.2940626 effect
(ZDCef*totdef_r)/100  #-13940.43 km2
#confidence interval G4
ZDCefconint <- (expm1(g4conint)*mnZDC) #+--0.1555742
ZDCefconint
(ZDCefconint*totdef_r)/100 #+-7515.015km2

#total impact if G4 had been 1: -24346.03km2 +- -13124.47
maxef <- (expm1(g4coef)*1) # -0.5135608 effect
(maxef*totdef_r)/100  # -24346.03 km2
#confidence interval G4
maxefconint <- (expm1(g4conint)*1) #+--0.1555742
maxefconint
(maxefconint*totdef_r)/100 #-7375.20
