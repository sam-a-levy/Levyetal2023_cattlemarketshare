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

# CREATE FIGURE 2 -  municipal market share over time  -----------------------------------------

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
                         #  legend,
                         #                       glegend,
                         nrow=3,rel_widths=0.3,rel_heights=0.3,
                         align="vh")

alltimestep
