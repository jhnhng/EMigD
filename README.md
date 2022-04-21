# trekR (Migration Classifier)

The aim of `trekR` is to automate the classification of individuals, as a migrant or non-migrant, across a continuous scale. I would recommend first looking over the article "Setting Up the Data"" under the articles tab, before looking at how to use the functions in the `trekR` package shown under "Using the functions in `trekR`".

To accomplish this:

  1. The collected movement data must first be segmented into the specified time periods (e.g., seasons, months, weeks, days).
  
  2. The segmented time periods are further broken down into specific intervals (e.g., a month can be broken down into weeks or a week can be broken down into
  days). 
  
  3. Fit and rasterize a home range for each interval from step 2. The output from this step should yield at least two utilization distributions (UD) that you     will then use in step 4.
  
  4. Calculate the Earth Mover's Distance (EMD) between two UDs created from step 3. The EMD is expected to be larger for migrants compared to residents. 
  
  5. Calculate "EM-Speeds" by dividing each EMD by the time elapsed between the two UDs.
  
  6. Calculate the average within- one specified period from step 1, and between- different periods. The ratios between these two different averages yields a 
  unitless value that will indicate where an individual is on the migratory spectrum. 



## Installation
To install the current development version of `trekR` use: `devtools::install_github("jhnhng/trekR")`

