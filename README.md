# Code for "The public health impact of fecal microbiota transplantation"

## File structure

The top level directory has a `Makefile`, which shows the analysis
dependencies, as well as the analysis scripts.

The `data/` folder has the parameter values from Rajasingham *et al*. The
`cache/` folder holds intermediate analyses, `fig/` shows output images, and
`output/` holds output tables.

The `Makefile` shows the analysis dependencies:

1. The Rajasingham *et al*. parameter values are used to estimate an *α0* for
   the Dirichlet distribution of nonsevere, severe, and fulminant initial CDI
2. That estimate for *α0* is used with other Rajasingham *et al*. parameters to
   generate our CDI model parameter distributions
3. The bootstrap variates for the input parameters are drawn
4. The effect of changing coverage is computed
5. The one-at-a-time sensitivity analysis is performed
6. The distributions of the outcome variables over the bootstrap variates are
   computed
7. The results of multiple analyses are summarized into a single file

## How to use

The analyses use R (developed against version 3.6.0) and the
[*tidyverse*](https://tidyverse.tidyverse.org/) packages. If you have a working
`Rscript` utility and have *tidyverse* installed, you should make able to
simply call `make`.

## Author

Scott Olesen <solesen@openbiome.org>
