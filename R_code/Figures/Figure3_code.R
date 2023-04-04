#' ---
#' Title: "Code for Figure 3"
#' Author: "SA Levy"
#' Date: 2023-03-21
#' ---

rm(list = ls())

if (!"data.io" %in% .packages()) {
  remotes::install_github("SciViews/data.io")
}

# packages
library(tidyverse)
library(sf)
library(geobr)
library(cowplot)
library(ggpubr)

select <- dplyr::select 

# Load data --------------------------------------------------------

# project data - obtainable at doi: 10.5281/zenodo.5105746
dat <- data.io::read$csv("https://zenodo.org/record/5105746/files/Levyetal2023_muni.csv?download=1") 
# Note: the URL corresponds to the 'Download' button on the page

# Prepare data for figures -----------------------------------------

# PREP FOR FIGURE 3

#create categorical dataset indicating if muni has >=50% G4/TAC or neither for fig 3
zd <- dat %>% 
  mutate(zd_ms = g4_ms+tac_ms,
         G4bin = ifelse(g4_ms>=0.5,"\u2265 50% G4",
                        ifelse(zd_ms<0.5,"< 50% either \n policy",NA)) %>% as.factor(),
         TACbin = ifelse(tac_ms>=0.5,"\u2265 50% TAC",
                         ifelse(zd_ms<0.5,"< 50% either \n policy",NA)) %>% as.factor(),
         def_rate = (defor/muni_area)*100,
  )

# CREATE FIGURE 3 -  deforestation by binary muni ZDC status  -----------------------------------------
# deforestation by muni with >= 50% G4/TAC & count of munis per group

#procedure: 
# 1) estimate deforestation for G4 & TAC vs. <50% neither policy
# 2) estimate number of municipalities in each group over time
# 3) bind together using ggpubr & cowplot packages
# 4) export

# pt1a: def rate where market share >= 50% - G4
g4_def_gp <- zd %>%
  mutate(G4bin=lag(G4bin)) %>%
  filter(year>2009 & !is.na(G4bin)) %>%
  mutate(year=year-1) %>%
  ggplot(aes(year,def_rate)) + geom_smooth(aes(color=G4bin,fill=G4bin),method="loess",alpha=0.37)+
  scale_x_continuous(breaks=c(2010,2012,2014,2016,2018),n.breaks=5)+labs(color="G4 coverage",fill="G4 coverage",x="Year",y="Deforestation rate (%)")+
  scale_y_continuous(breaks=c(-0.2,-0.1,0.0,0.1,0.2,0.3,0.4,0.5,0.6),n.breaks=9)+#limits=c(-.1,.55),
  coord_cartesian(ylim=c(-0,0.59), xlim = c(2009.6,2018.4),expand=FALSE)+                #coord_cartesian(expand=FALSE) needed  to expand without changing data
  scale_color_manual(values=c("#A7A7AA","#3283BE")) +scale_fill_manual(values=c("#A7A7AA","#3283BE")) +
  theme_light()+
  theme ( plot.margin = margin(t=0))

# pt1b: def rate where market share >= 50% - TAC
tc_def_gp <- zd %>%
  mutate(TACbin=lag(TACbin)) %>% #lag to match analysis
  filter(year>2009 & !is.na(TACbin)) %>%
  mutate(year=year-1) %>% # correct year to year of ZDC
  ggplot(aes(year,def_rate)) + geom_smooth(aes(color=TACbin,fill=TACbin),method="loess",alpha=0.37)+
  scale_x_continuous(breaks=c(2010,2012,2014,2016,2018),n.breaks=5)+labs(color="TAC coverage",fill="TAC coverage",x="Year",y="Deforestation rate (%)")+
  scale_y_continuous(breaks=c(-0.2,-0.1,0.0,0.1,0.2,0.3,0.4,0.5,0.6),n.breaks=9)+#limits=c(-.1,.55),
  coord_cartesian(ylim=c(-0,0.59), xlim = c(2009.6,2018.4),expand=FALSE)+                #coord_cartesian(expand=FALSE) needed  to expand without changing data
  scale_color_manual(values=c("#A7A7AA","#1A9993FF")) +scale_fill_manual(values=c("#A7A7AA","#1A9993FF")) +
  theme_light()+
  theme ( plot.margin = margin(t=0))

# pt2a: Count per group - G4
g4_count  <- zd %>%
  mutate(G4bin=lag(G4bin)) %>%
  filter(year>2009 & !is.na(G4bin)) %>%
  mutate(year=year-1) %>%
  ggplot(aes(year, fill=G4bin)) + geom_bar(stat = 'count', # Sidebar
                                           position='dodge', width=0.5, show.legend = F)+
  scale_fill_manual(values=c("#A7A7AA","#3283BE"))+
  coord_cartesian(xlim = c(2009.6,2018.4),expand=FALSE)+     
  scale_y_continuous(limits=c(0,220),breaks=c(0,100,200))+
  scale_x_continuous(breaks=c(2010,2012,2014,2016,2018),n.breaks=5)+
  ylab("Count")+
  theme_light()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.margin = margin(b=0))

# pt2b: Count per group - TAC
tc_count  <- zd %>%
  mutate(TACbin=lag(TACbin)) %>%
  filter(year>2009 & !is.na(TACbin)) %>%
  mutate(year=year-1) %>%
  group_by(TACbin,year) %>%
  summarise(count=n()) %>%   #rather than let R calculate the count as above, here I do manually so as to add a row
  ungroup() %>%
  add_row(year=2010,TACbin="≥ 50% TAC",count=0) %>%
  ggplot(aes(year, count, fill=TACbin)) + geom_bar(stat = 'identity', # Sidebar
                                                   position='dodge', width=0.5, show.legend = F)+
  scale_fill_manual(values=c("#A7A7AA","#1A9993FF"))+
  coord_cartesian(xlim = c(2009.6,2018.4),expand=FALSE)+     
  scale_y_continuous(limits=c(0,220),breaks=c(0,100,200))+
  scale_x_continuous(breaks=c(2010,2012,2014,2016,2018),n.breaks=5)+
  ylab("Count")+
  theme_light()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.margin = margin(b=0))


# pt 3: group the count & deforestation plots together
tac_grp <- ggarrange(tc_count+theme(axis.title.y = element_blank(),panel.grid.minor=element_blank()),
                     tc_def_gp+theme(axis.title.y = element_blank(),panel.grid.minor=element_blank()),
                     ncol=1,
                     nrow=2,
                     common.legend=T,
                     legend="top",
                     heights =  c(.25,.75),
                     align = 'v')

g4_grp <- ggarrange(g4_count,
                    g4_def_gp,
                    ncol=1,
                    nrow=2,
                    common.legend=T,
                    legend="top",
                    heights =  c(.25,.75),
                    align = 'v')

cowplot::plot_grid(g4_grp+theme(panel.border = element_blank()),
                   tac_grp+ theme(axis.title.y = element_blank(),panel.border = element_blank())
                   ,align = "h",nrow=1, labels = c("a.","b."),label_x=c(0.095,0.055),label_y = c(0.967,0.967),scale=0.95)

#pt 4: export
ggsave("Figures/Figure3.svg",
       scale=1,height=1780,width = 3302,unit="px")
