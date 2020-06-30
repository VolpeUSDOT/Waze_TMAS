# Get all necessary packages across data prep and analysis scripts 
# add any packages your scripts require here. Keep in alphabetical order.

loadpacks <- c(
  "doParallel",
  "foreach",
  "kableExtra",
  "knitr",
  "raster",
  "rgdal",
  "rgeos",
  "sp",
  "tidyverse",
  "usmap"
  )

for(i in loadpacks){
  if(length(grep(i, (.packages(all.available=T))))==0) install.packages(i, dependencies =TRUE)
  }
rm(i, loadpacks)
