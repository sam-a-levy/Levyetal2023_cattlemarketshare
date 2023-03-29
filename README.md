# Levyetal2021_cattlemarketshare
Stata files used to generate regression results and R code used to produce a) overall effects & b) all figures used in Levy et al. 2023's paper "Deforestation in the Brazilian Amazon could be halved by scaling up the implementation of zero-deforestation cattle commitments", published in Global Environmental Change.

This GitHub repository is intended to be used in conjunction with the data held at 10.5281/zenodo.5105746. All R scripts can be used to access the Zenodo data directly, but stata files must be run using downloaded files from Zenodo and the location of imput files (Levyetal2023_micro.csv/ Levyetal2023_muni.csv; line 5 in both stata files) changed manually. Additionally, Figure 2 from the publication was finished in adobe illustrator & the code here only generates the maps used, rather than the full figure.

## Contents

### Stata Code:
- **Levyetal2021_regressions_muni.do** produces municipal level econometric regression results used in both main text and supplementary material
- **Levyetal2021_regressions_micro.do** produces microregion level econometric regression results used in supplementary material

### R Code:
- totaleffect.R calculates the total effects reported in the paper, based on results from Levyetal2021_regressions_muni.do
- Figure1_code.R generates Figure 1 from Levy et al. 2023
- Figure2_code.R generates Figure 2 from Levy et al. 2023 (note: this figure was finished in adobe illustrator)
- Figure3_code.R generates Figure 3 from Levy et al. 2023

For a description of the variables used in the figures and analyses see Levy et al. 2023 or the README file included in Zenodo.
