#' ---
#' Title: "Code for Figure 1"
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

# Load data --------------------------------------------------------

# project data - obtainable at doi: 10.5281/zenodo.5105746
SH <- data.io::read$csv("https://zenodo.org/record/5105746/files/Levyetal2023_Slaughterhouse.csv?download=1") 
# Note: the URL corresponds to the 'Download' button on the page

#external data - obtainable at: https://supplychains.trase.earth/data (select "BRAZIL - BEEF (ALL YEARS)")
trase_files_location <- "~/Downloads/BRAZIL_BEEF_2.0.2_pc"  #NOTE: YOU MUST SET THIS YOURSELF
Trase <- map_dfr(list.files(trase_files_location,pattern=".csv",full.names = TRUE),read_csv)

palette <- set_names(c("#A7A7AA","#3283BE","#1A9993FF"),nm=c("No ZDC","G4","TAC"))

# Prepare data for figures -----------------------------------------

# PREP FOR FIGURE 1

#create ZDC factor for slaughterhouse level dataset
shouse <- SH %>%
  mutate(ZDC=ifelse(G4==1,"G4",ifelse(TAC==1,"TAC","No ZDC")) %>%
           factor(levels=c("No ZDC","TAC","G4")),ordered=TRUE) 

# Change Trase classification to match paper (i.e. treat any volume covered by G4 (i.e., G4 & TAC) as G4 only) & 
# summarise to total over period, for study states
trs <- Trase %>% 
  filter(STATE %in% c("MATO GROSSO","RONDONIA","PARA")) %>%
  mutate(ZDC= ifelse(str_detect(ZERO_DEFORESTATION_BRAZIL_BEEF,"G4"),"G4",
                     ifelse(ZERO_DEFORESTATION_BRAZIL_BEEF=="NONE","No ZDC",ZERO_DEFORESTATION_BRAZIL_BEEF)) %>%
           factor(levels=c("No ZDC","TAC","G4")),ordered=TRUE) %>%
  group_by(ZDC) %>%
  summarise(vol_exp = sum(BEEF_EQUIVALENT_TONNES,na.rm=TRUE)) %>%
  mutate(perc_exp=vol_exp/sum(vol_exp)) 

# summarize slaughterhouse-level data to state-level dataset
# note: need to fill in missing years for figure tidyness
shouse_state <- shouse %>%
  group_by(ZDC,STATE, year) %>%
  summarise(vol_tot = sum(vol,na.rm=TRUE)) %>%
  group_by(year,STATE) %>%
  mutate(perc_tot_state = vol_tot/sum(vol_tot)*100) %>%
  mutate(perc_tot_state = vol_tot/sum(vol_tot)*100) %>%
  ungroup() %>%
  add_row(ZDC="TAC",STATE="MT",year=2012,vol_tot=0,perc_tot_state=0) %>%
  add_row(ZDC="TAC",STATE="RO",year=2013,vol_tot=0,perc_tot_state=0) %>%
  mutate(ZDC = factor(ZDC,levels=c("No ZDC","TAC","G4")),ordered=TRUE)

#summarize slaughterhouse-level data to overall level dataset & join with Trase data
shouse_tot <- shouse %>% filter(year %in% 2015:2017) %>%
  group_by(ZDC) %>%
  summarise(vol_tot = sum(vol,na.rm=TRUE)) %>%
  mutate(perc_tot = vol_tot/sum(vol_tot)) %>%
  left_join(trs)

#convert to long format & add a column for ZDC vs. No ZDC
shouse_tot_lng <- shouse_tot %>%
  mutate(ZD = ifelse(ZDC!="No ZDC","ZDC","No ZDC")) %>%
  pivot_longer(cols=-c(ZDC,ZD),names_pattern = "(.*)_(.*)",names_to=c(".value","type")) %>%
  mutate(ZD   = factor(ZD, levels=c("No ZDC","ZDC")),
         ZDC  = factor(ZDC,levels=c("TAC","G4","No ZDC")),
         type = factor(type,levels=c("tot","exp",labels="Total production","Sales")),
         perc = perc*100)

# CREATE FIGURE 1 -  market share over time -----------------------------------------


#procedure: 
# 1) create geom_area() plots for each state to give production volumes over time
# 2) create geom_area() plots for whole study period to give production volumes over time
# 3) create stacked production volumes for production/export by ZDC
# 4) bind together using cowplot packages 
# 5) export

# pt1a: geom_area() for Rondonia
RO_stack<- shouse_state %>% filter(STATE=="RO") %>%
  ggplot(aes(x=year,y=perc_tot_state,fill=ZDC)) +
  scale_fill_manual(values=palette)+
  geom_area(color="black",alpha=0.5,linewidth=0.25) +labs( y="", x="Year",fill="ZDC policy")+  theme_bw() + 
  coord_cartesian(expand=F)

# pt1b: geom_area() for Pará
PA_stack <- shouse_state %>% filter(STATE=="PA") %>%
  ggplot(aes(x=year,y=perc_tot_state,fill=ZDC)) +
  scale_fill_manual(values=palette)+
  geom_area(color="black",alpha=0.5,size=0.25) +labs( y="Market share (%)", x="Year",fill="ZDC policy")+  theme_bw() + 
  coord_cartesian(expand=F)

# pt1c: geom_area() for Mato Grosso
MT_stack <- shouse_state %>% filter(STATE=="MT") %>%
  ggplot(aes(x=year,y=perc_tot_state,fill=ZDC)) +
  scale_fill_manual(values=palette)+
  # scale_x_continuous(breaks=c(2011,2013,2015,2017))+
  geom_area(color="black",alpha=0.5,size=0.25) +labs( y="", x="Year",fill="ZDC policy")+  theme_bw() + 
  coord_cartesian(expand=F)

# pt2: geom_area() for study region 
# note: have to process shouse_state a little further for this
tot_stack <- shouse_state %>%
  group_by(year,ZDC) %>%
  summarise(vol=sum(vol_tot)) %>%
  group_by(year) %>%
  mutate(perc_tot_fl=(vol/sum(vol))*100) %>%
  ggplot(aes(x=year,y=perc_tot_fl,fill=ZDC)) +
  scale_fill_manual(values=palette)+
  # scale_x_continuous(breaks=c(2011,2013,2015,2017))+
  geom_area(color="black",alpha=0.5,size=0.25) +labs(y="Market share (%)", x="Year",fill="ZDC policy")+  theme_bw() + 
  coord_cartesian(expand=F)

# pt3: stacked production/export volumes  
exp_dom <- shouse_tot_lng %>%
  ggplot(aes(fill=ZDC,y=perc,x=type)) + 
  geom_bar(position="stack",color="black",stat="identity",alpha=0.6,size=0.25 ) +
  scale_y_continuous(breaks=c(0,25,50,75,100),limits = c(0,100),expand=c(0,0))+
  facet_wrap(~ZD)+
  scale_x_discrete(labels=c("tot"="Total sales","exp"="Exports"))+
  scale_fill_manual(values=palette)+
  theme_bw()+
  xlab("Sale type")+
  ylab( "Share of sale volume (%)") +
  theme(panel.spacing = grid::unit(0, "lines"),
        panel.border = element_blank(),
        strip.text.x=element_blank()
  ) 

#pt4a: merge state level geom_area() plots together
stacks <- cowplot::plot_grid(PA_stack + ggtitle("Pará") + 
                               theme(plot.title = element_text(hjust = 0.5),legend.position = "none",panel.grid.minor=element_blank(),
                                     panel.background = element_rect(fill = "transparent", colour = NA),  
                                     plot.background = element_rect(fill = "transparent", colour = NA)),
                             RO_stack + ggtitle("Rondônia") + 
                               theme(plot.title = element_text(hjust = 0.5),legend.position = "none",panel.grid.minor=element_blank(),
                                     panel.background = element_rect(fill = "transparent", colour = NA),  
                                     plot.background = element_rect(fill = "transparent", colour = NA)),
                             MT_stack+ ggtitle("Mato Grosso") + 
                               theme(plot.title = element_text(hjust = 0.5),legend.position = "none",panel.grid.minor=element_blank(),
                                     panel.background = element_rect(fill = "transparent", colour = NA),  
                                     plot.background = element_rect(fill = "transparent", colour = NA)),
                             nrow=1,labels = c("c.","d.","e."), label_y = 1,label_size = 11
                             #,label_size = 18
)


# pt4b: merge total study region geom_area() & production/export plots together
toprow<- cowplot::plot_grid(tot_stack+ ggtitle("Over time")+
                              theme(plot.title = element_text(hjust = 0.5),panel.grid.minor=element_blank(),
                                    legend.position = "none",panel.background = element_rect(fill = "transparent", colour = NA),  
                                    plot.background = element_rect(fill = "transparent", colour = NA)),
                            exp_dom+ ggtitle("Total vs. exports")+
                              theme(plot.title = element_text(hjust = 0.5),panel.grid.minor=element_blank(),legend.position = "none",axis.title.y = element_blank(),panel.background = element_rect(fill = "transparent", colour = NA),  
                                    plot.background = element_rect(fill = "transparent", colour = NA)),
                            #cowplot::get_legend(all_dens),
                            axis="l",
                            align="h",
                            labels=c("a.","b."), nrow=1,greedy = F,label_size = 11
                            #,rel_wids=c(3,3,1)
)

# pt4c: add subtitles
title1 <- cowplot::ggdraw() + 
  cowplot::draw_label("Zero-deforestation commitment market share across whole region",fontface = 'bold',hjust = 0.45)
title2 <- cowplot::ggdraw() + 
  cowplot::draw_label("Zero-deforestation commitment market share per state",fontface = 'bold',hjust = 0.5)

# pt 4d: merge all together
toprow_t<-cowplot::plot_grid(title1,toprow,ncol=1,rel_heights=c(0.1,1))
stacks_t<-cowplot::plot_grid(title2,stacks,ncol=1,rel_heights=c(0.1,1))
fig_t<-cowplot::plot_grid(toprow_t,stacks_t,nrow=2,rel_heights=c(1,0.8))

cowplot::plot_grid(fig_t,cowplot::get_legend(RO_stack),rel_widths=c(1,0.15))

# pt5: export
ggsave("Figures/Figure1.svg",
       scale=1,height=2166,width = 3168,unit="px")
