#' ---
#' Title: "Code for Figure 2"
#' Author: "SA Levy"
#' Date: 2023-03-21
#' ---

rm(list = ls())
# packages
library(tidyverse)
library(sf)
library(geobr)
library(cowplot)
library(ggpubr)

select <- dplyr::select

# NOTE: THIS FIGURE WAS FINALIZED IN ADOBE ILLUSTRATOR.
# Creation of legend, column & row labels was done outside R. This code is only used to generate & map values 

# Load data --------------------------------------------------------

# project data - obtainable at doi: 10.5281/zenodo.5105746
dat_marketsh <- read_csv("/Users/sal9799/Downloads/rstudio-export/Levyetal2023_marketsharefiguredata.csv")
SH  <- read_csv("~/Downloads/Levyetal2023_Slaughterhouse.csv")

#brazil geo data - taken fom IBGE using geobr package 
munis <- read_municipality(year=2010,simplified = TRUE)
state <- read_state(year = 2010,simplified = TRUE)
brazil <- read_country(simplified = TRUE)

# brazil biome data - taken fom IBGE via GFW for biomes as 2004 biome boundaries includes rivers etc.
biomes <- st_read("http://data.globalforestwatch.org/datasets/54ec099791644be4b273d9d8a853d452_4.geojson") %>% st_transform(crs=st_crs(munis))

#am boundary line for study region - taken from github, pre-processed by author
am_line <- st_read("Supporting_files/am_line.geojson")

# Prepare data for figures -----------------------------------------

# PREP FOR FIGURE 2

# filter to study region
state_study_r <- state %>% filter(code_state %in% c(11,51,15))
am <- biomes %>% filter(name=="Amazônia")
munis_study_r <- munis %>% filter(code_state %in% c(11,15,51))

#convert SH to spatial object
sh_spat <- SH %>%
  mutate(ZDC_lg=  G4==1|TAC==1,
         G4_lg =  G4==1,
         TAC_lg = TAC==1) %>%
  st_as_sf(coords=c("long","lat"),crs=st_crs(munis))

#examine
#ggplot() +
#  geom_sf(data=state_study_r) +
#  geom_sf(data=sh_spat %>% filter(year==2010 & X2010==1)) +
#  geom_sf(data=am_line,color="red")

#change to wideform & create categories for figure

#define 7 equal breaks
equal.interval <- c( 0.0000000, 0.1666667, 0.3333333, 0.5000000, 0.6666667, 0.8333333, 1.0000000) 

# convert to wide format with equal break categories for selected years (2010,2013,2016,2018)
dat_wide <- dat_marketsh %>%
  mutate(across(contains("ms"),~ifelse(Amazon==0,NA,.)),
         zd_ms = tac_ms+g4_ms) %>%
  select(code_ibge, year,g4_ms,tac_ms,zd_ms) %>%
  mutate(across(contains("ms"),~cut(.,breaks=equal.interval,include.lowest=TRUE),.names="{.col}_cat")) %>%
  filter(year %in% c(2010,2013,2016,2018)) %>%
  pivot_wider(id_cols=code_ibge, 
              names_from = year, 
              values_from = contains("ms"), 
              names_sep = ".")

#join to spat dat
spat_dat_wide <- munis_study_r %>% left_join(by=c("code_muni"="code_ibge"),dat_wide)
  
# CREATE FIGURE 2 -  municipal market share over time  -----------------------------------------

z2010 <- ggplot() +
  geom_sf(data=spat_dat_wide, aes(fill = zd_ms_cat.2010,geometry=geom),color="#EDEDED",size=0.2) +
  scale_fill_manual(name = "ZDC market share",values = c( "#EFF3FF", "#C6DBEF", "#9ECAE1", "#6BAED6", "#3182BD","#08519C"),
                    labels=c("0% - 17%","18% - 33%","34% - 50%","51% - 67%","67% - 83%","83%-100%"),
                    na.value = "#D6D5D5") + 
  coord_equal() +
  geom_sf(data=state_study_r,fill=NA) +
  geom_sf(data=am_line,colour="red") +
  geom_sf(data=sh_spat %>% filter(year==2010 & X2010==1 & INSPECT=="other"),aes(color=ZDC_lg),shape=18,size=1.7) +
  geom_sf(data=sh_spat %>% filter(year==2010 & X2010==1 & INSPECT=="SIE"),aes(color=ZDC_lg),shape=19,size=2) +
  geom_sf(data=sh_spat %>% filter(year==2010 & X2010==1 & INSPECT=="SIF"),aes(color=ZDC_lg),shape=17,size=3) +
  scale_color_manual(name = "ZDC type", values = c("TRUE" = '#FFE700',"FALSE" = "#F6A316"), 
                     labels = c("No ZDC","ZDC"))+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),plot.title = element_text(hjust = 0.5),
                     axis.title=element_blank(),axis.text.x = element_blank(),
                     axis.text.y = element_blank(),axis.ticks = element_blank())


z2013 <- ggplot() +
  geom_sf(data=spat_dat_wide, aes(fill = zd_ms_cat.2013,geometry=geom),color="#EDEDED",size=0.2) +
  scale_fill_manual(name = "ZDC market share",values = c( "#EFF3FF", "#C6DBEF", "#9ECAE1", "#6BAED6", "#3182BD","#08519C"),
                    labels=c("0% - 17%","18% - 33%","34% - 50%","51% - 67%","67% - 83%","83%-100%"),
                    na.value = "#D6D5D5") + 
  coord_equal() +
  geom_sf(data=state_study_r,fill=NA) +
  geom_sf(data=am_line,colour="red") +
  geom_sf(data=sh_spat %>% filter(year==2013 & X2013==1 & INSPECT=="other"),aes(color=ZDC_lg),shape=18,size=1.7) +
  geom_sf(data=sh_spat %>% filter(year==2013 & X2013==1 & INSPECT=="SIE"),aes(color=ZDC_lg),shape=19,size=2) +
  geom_sf(data=sh_spat %>% filter(year==2013 & X2013==1 & INSPECT=="SIF"),aes(color=ZDC_lg),shape=17,size=3) +
  scale_color_manual(name = "ZDC type", values = c("TRUE" = '#FFE700',"FALSE" = "#F6A316"), 
                     labels = c("No ZDC","ZDC"))+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),plot.title = element_text(hjust = 0.5),
                     axis.title=element_blank(),axis.text.x = element_blank(),
                     axis.text.y = element_blank(),axis.ticks = element_blank())

z2016 <- ggplot() +
  geom_sf(data=spat_dat_wide, aes(fill = zd_ms_cat.2016,geometry=geom),color="#EDEDED",size=0.2) +
  scale_fill_manual(name = "ZDC market share",values = c( "#EFF3FF", "#C6DBEF", "#9ECAE1", "#6BAED6", "#3182BD","#08519C"),
                    labels=c("0% - 17%","18% - 33%","34% - 50%","51% - 67%","67% - 83%","83%-100%"),
                    na.value = "#D6D5D5") + 
  coord_equal() +
  geom_sf(data=state_study_r,fill=NA) +
  geom_sf(data=am_line,colour="red") +
  geom_sf(data=sh_spat %>% filter(year==2016 & X2016==1 & INSPECT=="other"),aes(color=ZDC_lg),shape=18,size=1.7) +
  geom_sf(data=sh_spat %>% filter(year==2016 & X2016==1 & INSPECT=="SIE"),aes(color=ZDC_lg),shape=19,size=2) +
  geom_sf(data=sh_spat %>% filter(year==2016 & X2016==1 & INSPECT=="SIF"),aes(color=ZDC_lg),shape=17,size=3) +
  scale_color_manual(name = "ZDC type", values = c("TRUE" = '#FFE700',"FALSE" = "#F6A316"), 
                     labels = c("No ZDC","ZDC"))+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),plot.title = element_text(hjust = 0.5),
                     axis.title=element_blank(),axis.text.x = element_blank(),
                     axis.text.y = element_blank(),axis.ticks = element_blank())

z2018 <- ggplot() +
  geom_sf(data=spat_dat_wide, aes(fill = zd_ms_cat.2018,geometry=geom),color="#EDEDED",size=0.2) +
  scale_fill_manual(name = "ZDC market share",values = c( "#EFF3FF", "#C6DBEF", "#9ECAE1", "#6BAED6", "#3182BD","#08519C"),
                    labels=c("0% - 17%","18% - 33%","34% - 50%","51% - 67%","67% - 83%","83%-100%"),
                    na.value = "#D6D5D5") + 
  coord_equal() +
  geom_sf(data=state_study_r,fill=NA) +
  geom_sf(data=am_line,colour="red") +
  geom_sf(data=sh_spat %>% filter(year==2018 & X2018==1 & INSPECT=="other"),aes(color=ZDC_lg),shape=18,size=1.7) +
  geom_sf(data=sh_spat %>% filter(year==2018 & X2018==1 & INSPECT=="SIE"),aes(color=ZDC_lg),shape=19,size=2) +
  geom_sf(data=sh_spat %>% filter(year==2018 & X2018==1 & INSPECT=="SIF"),aes(color=ZDC_lg),shape=17,size=3) +
  scale_color_manual(name = "ZDC type", values = c("TRUE" = '#FFE700',"FALSE" = "#F6A316"), 
                     labels = c("No ZDC","ZDC"))+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),plot.title = element_text(hjust = 0.5),
                     axis.title=element_blank(),axis.text.x = element_blank(),
                     axis.text.y = element_blank(),axis.ticks = element_blank())


g2010 <- ggplot() +
  geom_sf(data=spat_dat_wide, aes(fill = g4_ms_cat.2010,geometry=geom),color="#EDEDED",size=0.2) +
  scale_fill_manual(name = "G4 market share",values = c( "#EFF3FF", "#C6DBEF", "#9ECAE1", "#6BAED6", "#3182BD","#08519C"),
                    labels=c("0% - 17%","18% - 33%","34% - 50%","51% - 67%","67% - 83%","83%-100%"),
                    na.value = "#D6D5D5") + 
  coord_equal() +
  geom_sf(data=state_study_r,fill=NA) +
  geom_sf(data=am_line,colour="red") +
  geom_sf(data=sh_spat %>% filter(year==2010 & X2010==1 & INSPECT=="other"),aes(color=G4_lg),shape=18,size=1.7) +
  geom_sf(data=sh_spat %>% filter(year==2010 & X2010==1 & INSPECT=="SIE"),aes(color=G4_lg),shape=19,size=2) +
  geom_sf(data=sh_spat %>% filter(year==2010 & X2010==1 & INSPECT=="SIF"),aes(color=G4_lg),shape=17,size=3) +
  scale_color_manual(name = "G4 type", values = c("TRUE" = '#FFE700',"FALSE" = "#F6A316"), 
                     labels = c("No G4","G4"))+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),plot.title = element_text(hjust = 0.5),
                     axis.title=element_blank(),axis.text.x = element_blank(),
                     axis.text.y = element_blank(),axis.ticks = element_blank())


g2013 <- ggplot() +
  geom_sf(data=spat_dat_wide, aes(fill = g4_ms_cat.2013,geometry=geom),color="#EDEDED",size=0.2) +
  scale_fill_manual(name = "G4 market share",values = c( "#EFF3FF", "#C6DBEF", "#9ECAE1", "#6BAED6", "#3182BD","#08519C"),
                    labels=c("0% - 17%","18% - 33%","34% - 50%","51% - 67%","67% - 83%","83%-100%"),
                    na.value = "#D6D5D5") + 
  coord_equal() +
  geom_sf(data=state_study_r,fill=NA) +
  geom_sf(data=am_line,colour="red") +
  geom_sf(data=sh_spat %>% filter(year==2013 & X2013==1 & INSPECT=="other"),aes(color=G4_lg),shape=18,size=1.7) +
  geom_sf(data=sh_spat %>% filter(year==2013 & X2013==1 & INSPECT=="SIE"),aes(color=G4_lg),shape=19,size=2) +
  geom_sf(data=sh_spat %>% filter(year==2013 & X2013==1 & INSPECT=="SIF"),aes(color=G4_lg),shape=17,size=3) +
  scale_color_manual(name = "G4 type", values = c("TRUE" = '#FFE700',"FALSE" = "#F6A316"), 
                     labels = c("No G4","G4"))+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),plot.title = element_text(hjust = 0.5),
                     axis.title=element_blank(),axis.text.x = element_blank(),
                     axis.text.y = element_blank(),axis.ticks = element_blank())

g2016 <- ggplot() +
  geom_sf(data=spat_dat_wide, aes(fill = g4_ms_cat.2016,geometry=geom),color="#EDEDED",size=0.2) +
  scale_fill_manual(name = "G4 market share",values = c( "#EFF3FF", "#C6DBEF", "#9ECAE1", "#6BAED6", "#3182BD","#08519C"),
                    labels=c("0% - 17%","18% - 33%","34% - 50%","51% - 67%","67% - 83%","83%-100%"),
                    na.value = "#D6D5D5") + 
  coord_equal() +
  geom_sf(data=state_study_r,fill=NA) +
  geom_sf(data=am_line,colour="red") +
  geom_sf(data=sh_spat %>% filter(year==2016 & X2016==1 & INSPECT=="other"),aes(color=G4_lg),shape=18,size=1.7) +
  geom_sf(data=sh_spat %>% filter(year==2016 & X2016==1 & INSPECT=="SIE"),aes(color=G4_lg),shape=19,size=2) +
  geom_sf(data=sh_spat %>% filter(year==2016 & X2016==1 & INSPECT=="SIF"),aes(color=G4_lg),shape=17,size=3) +
  scale_color_manual(name = "G4 type", values = c("TRUE" = '#FFE700',"FALSE" = "#F6A316"), 
                     labels = c("No G4","G4"))+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),plot.title = element_text(hjust = 0.5),
                     axis.title=element_blank(),axis.text.x = element_blank(),
                     axis.text.y = element_blank(),axis.ticks = element_blank())

g2018 <- ggplot() +
  geom_sf(data=spat_dat_wide, aes(fill = g4_ms_cat.2018,geometry=geom),color="#EDEDED",size=0.2) +
  scale_fill_manual(name = "G4 market share",values = c( "#EFF3FF", "#C6DBEF", "#9ECAE1", "#6BAED6", "#3182BD","#08519C"),
                    labels=c("0% - 17%","18% - 33%","34% - 50%","51% - 67%","67% - 83%","83%-100%"),
                    na.value = "#D6D5D5") + 
  coord_equal() +
  geom_sf(data=state_study_r,fill=NA) +
  geom_sf(data=am_line,colour="red") +
  geom_sf(data=sh_spat %>% filter(year==2018 & X2018==1 & INSPECT=="other"),aes(color=G4_lg),shape=18,size=1.7) +
  geom_sf(data=sh_spat %>% filter(year==2018 & X2018==1 & INSPECT=="SIE"),aes(color=G4_lg),shape=19,size=2) +
  geom_sf(data=sh_spat %>% filter(year==2018 & X2018==1 & INSPECT=="SIF"),aes(color=G4_lg),shape=17,size=3) +
  scale_color_manual(name = "G4 type", values = c("TRUE" = '#FFE700',"FALSE" = "#F6A316"), 
                     labels = c("No G4","G4"))+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),plot.title = element_text(hjust = 0.5),
                     axis.title=element_blank(),axis.text.x = element_blank(),
                     axis.text.y = element_blank(),axis.ticks = element_blank())


t2010 <- ggplot() +
  geom_sf(data=spat_dat_wide, aes(fill = tac_ms_cat.2010,geometry=geom),color="#EDEDED",size=0.2) +
  scale_fill_manual(name = "TAC market share",values = c( "#EFF3FF", "#C6DBEF", "#9ECAE1", "#6BAED6", "#3182BD","#08519C"),
                    labels=c("0% - 17%","18% - 33%","34% - 50%","51% - 67%","67% - 83%","83%-100%"),
                    na.value = "#D6D5D5") + 
  coord_equal() +
  geom_sf(data=state_study_r,fill=NA) +
  geom_sf(data=am_line,colour="red") +
  geom_sf(data=sh_spat %>% filter(year==2010 & X2010==1 & INSPECT=="other"),aes(color=TAC_lg),shape=18,size=1.7) +
  geom_sf(data=sh_spat %>% filter(year==2010 & X2010==1 & INSPECT=="SIE"),aes(color=TAC_lg),shape=19,size=2) +
  geom_sf(data=sh_spat %>% filter(year==2010 & X2010==1 & INSPECT=="SIF"),aes(color=TAC_lg),shape=17,size=3) +
  scale_color_manual(name = "TAC type", values = c("TRUE" = '#FFE700',"FALSE" = "#F6A316"), 
                     labels = c("No TAC","TAC"))+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),plot.title = element_text(hjust = 0.5),
                     axis.title=element_blank(),axis.text.x = element_blank(),
                     axis.text.y = element_blank(),axis.ticks = element_blank())


t2013 <- ggplot() +
  geom_sf(data=spat_dat_wide, aes(fill = tac_ms_cat.2013,geometry=geom),color="#EDEDED",size=0.2) +
  scale_fill_manual(name = "TAC market share",values = c( "#EFF3FF", "#C6DBEF", "#9ECAE1", "#6BAED6", "#3182BD","#08519C"),
                    labels=c("0% - 17%","18% - 33%","34% - 50%","51% - 67%","67% - 83%","83%-100%"),
                    na.value = "#D6D5D5") + 
  coord_equal() +
  geom_sf(data=state_study_r,fill=NA) +
  geom_sf(data=am_line,colour="red") +
  geom_sf(data=sh_spat %>% filter(year==2013 & X2013==1 & INSPECT=="other"),aes(color=TAC_lg),shape=18,size=1.7) +
  geom_sf(data=sh_spat %>% filter(year==2013 & X2013==1 & INSPECT=="SIE"),aes(color=TAC_lg),shape=19,size=2) +
  geom_sf(data=sh_spat %>% filter(year==2013 & X2013==1 & INSPECT=="SIF"),aes(color=TAC_lg),shape=17,size=3) +
  scale_color_manual(name = "TAC type", values = c("TRUE" = '#FFE700',"FALSE" = "#F6A316"), 
                     labels = c("No TAC","TAC"))+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),plot.title = element_text(hjust = 0.5),
                     axis.title=element_blank(),axis.text.x = element_blank(),
                     axis.text.y = element_blank(),axis.ticks = element_blank())

t2016 <- ggplot() +
  geom_sf(data=spat_dat_wide, aes(fill = tac_ms_cat.2016,geometry=geom),color="#EDEDED",size=0.2) +
  scale_fill_manual(name = "TAC market share",values = c( "#EFF3FF", "#C6DBEF", "#9ECAE1", "#6BAED6", "#3182BD","#08519C"),
                    labels=c("0% - 17%","18% - 33%","34% - 50%","51% - 67%","67% - 83%","83%-100%"),
                    na.value = "#D6D5D5") + 
  coord_equal() +
  geom_sf(data=state_study_r,fill=NA) +
  geom_sf(data=am_line,colour="red") +
  geom_sf(data=sh_spat %>% filter(year==2016 & X2016==1 & INSPECT=="other"),aes(color=TAC_lg),shape=18,size=1.7) +
  geom_sf(data=sh_spat %>% filter(year==2016 & X2016==1 & INSPECT=="SIE"),aes(color=TAC_lg),shape=19,size=2) +
  geom_sf(data=sh_spat %>% filter(year==2016 & X2016==1 & INSPECT=="SIF"),aes(color=TAC_lg),shape=17,size=3) +
  scale_color_manual(name = "TAC type", values = c("TRUE" = '#FFE700',"FALSE" = "#F6A316"), 
                     labels = c("No TAC","TAC"))+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),plot.title = element_text(hjust = 0.5),
                     axis.title=element_blank(),axis.text.x = element_blank(),
                     axis.text.y = element_blank(),axis.ticks = element_blank())

t2018 <- ggplot() +
  geom_sf(data=spat_dat_wide, aes(fill = tac_ms_cat.2018,geometry=geom),color="#EDEDED",size=0.2) +
  scale_fill_manual(name = "TAC market share",values = c( "#EFF3FF", "#C6DBEF", "#9ECAE1", "#6BAED6", "#3182BD","#08519C"),
                    labels=c("0% - 17%","18% - 33%","34% - 50%","51% - 67%","67% - 83%","83%-100%"),
                    na.value = "#D6D5D5") + 
  coord_equal() +
  geom_sf(data=state_study_r,fill=NA) +
  geom_sf(data=am_line,colour="red") +
  geom_sf(data=sh_spat %>% filter(year==2018 & X2018==1 & INSPECT=="other"),aes(color=TAC_lg),shape=18,size=1.7) +
  geom_sf(data=sh_spat %>% filter(year==2018 & X2018==1 & INSPECT=="SIE"),aes(color=TAC_lg),shape=19,size=2) +
  geom_sf(data=sh_spat %>% filter(year==2018 & X2018==1 & INSPECT=="SIF"),aes(color=TAC_lg),shape=17,size=3) +
  scale_color_manual(name = "TAC type", values = c("TRUE" = '#FFE700',"FALSE" = "#F6A316"), 
                     labels = c("No TAC","TAC"))+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),plot.title = element_text(hjust = 0.5),
                     axis.title=element_blank(),axis.text.x = element_blank(),
                     axis.text.y = element_blank(),axis.ticks = element_blank())

alltimestep <- plot_grid(z2010 + theme(legend.position = "none"),
                         z2013 + theme(legend.position = "none"),
                         z2016 + theme(legend.position = "none"),
                         z2018 + theme(legend.position = "none"),
                         g2010 + theme(legend.position = "none"),
                         g2013 + theme(legend.position = "none"),
                         g2016 + theme(legend.position = "none"),
                         g2018 + theme(legend.position = "none"),
                         t2010 + theme(legend.position = "none"),
                         t2013 + theme(legend.position = "none"),
                         t2016 + theme(legend.position = "none"),
                         t2018 + theme(legend.position = "none"),
                         nrow=3,rel_widths=0.3,rel_heights=0.3,
                         align="vh")

alltimestep

#pt 4: export
ggsave("Figures/Figure2.svg",
       scale=1,height=3162,width = 3162,unit="px")

