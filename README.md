# Levyetal2021_cattlemarketshare
Stata files used to generate regression results and R code used to produce a) overall effects & b) all figures used in Levy et al. 2023's paper "Deforestation in the Brazilian Amazon could be halved by scaling up the implementation of zero-deforestation cattle commitments", published in Global Environmental Change.

## Using the Github repository
This GitHub repository is intended to be used in conjunction with the data held at 10.5281/zenodo.5105746. All R scripts can be used to access the Zenodo data directly, assuming that the package `data.io` is installed. 

To execute the Stata files included in this repository, you must first download the Zenodo locally and specify the location for each Stata do file (Levyetal2023_micro.csv/ Levyetal2023_muni.csv; line 5 in both Stata files). 

Additionally, Figure 2 from the publication was finished in Adobe Illustrator & the code here only generates the maps used, rather than the full figure. 

## Contents

### Stata Code:
- **Levyetal2021_regressions_muni.do** produces municipal level econometric regression results used in both main text and supplementary material
- **Levyetal2021_regressions_micro.do** produces microregion level econometric regression results used in supplementary material

### R Code:
- **totaleffect.R** calculates the total effects reported in the paper, based on results from Levyetal2021_regressions_muni.do
- **Figure1_code.R** generates Figure 1 from Levy et al. 2023
- **Figure2_code.R** generates Figure 2 from Levy et al. 2023 (note: this figure was finished in Adobe Illustrator & uses files sourced from IBGE's API via the `geobr` package)
- **Figure3_code.R** generates Figure 3 from Levy et al. 2023

### Figures:
- **Figure1.svg** is the SVG file of Figure 1 used in Levy et al. 2023, produced by Figure1_code.R. This file is also used to calculate G4 and TAC market share over time and for the entire study period as reported in Levy et al. 2023.
- **Figure2.svg** is the SVG file of Figure 2 used in Levy et al. 2023, produced by Figure2_code.R. Additional work was done on this figure in Adobe Illustrator. The Illustrator output is provided in Figure2_final.jpg & Figure2_final.pdf
- **Figure3.svg** is the SVG file of Figure 3 used in Levy et al. 2023, produced by Figure3_code.R.

### Supporting_files:
- **am_line.geojson** the Amazon biome boundary for the RO, MT, PA study region, based on IBGE's 2004 biome boundaries (the boundaries in use at the start of the study period).

For a description of the variables used in the figures and analyses see Levy et al. 2023 or the README file included in Zenodo.
