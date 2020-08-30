# gaen-attenuation

This repo contains utility functions to process signal strength data gathered with the University of Arizona to test GAEN.

Developed using R version 3.6.3 with RStudio (it will most likely work with other R versions as well).

Requires the following R libraries: readxl, data.table, rjson, plyr, stringr, foreach, ggplot2, ggforce, coefplot, Rmisc, gtools

The script takes a minute or so to process all the raw experimental data, combine the results, and plot the data in a notebook.

To run, 'knit' the file AllTestData.Rmd. This will save the processed data to a csv and it will display a notebook showing simple plots of the data.
