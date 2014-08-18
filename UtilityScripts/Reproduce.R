
###################################
### Reproducible Research
###################################
# When executed by R, this file will manipulate the original data sources (ie, ZZZZ)
# to produce a groomed dataset suitable for analysis and graphing.

###################################
# Clear memory from previous runs
base::rm(list=base::ls(all=TRUE))

###################################
# Verify the working directory has been set correctly.  Much of the code assumes the working directory is the repository's root directory.
# In the following line, rename `LentaPredictionPhase1` to your repository.
if( base::basename(base::getwd()) != "LentaPredictionPhase1" ) {
  base::stop("The working directory should be set to the root of the package/repository.  ",
       "It's currently set to `", base::getwd(), "`.")
}
###################################
# Install the necessary packages.
pathInstallPackages <- "./UtilityScripts/InstallPackages.R"
if( !file.exists(pathInstallPackages)) {
  base::stop("The file `", pathInstallPackages, "` was not found.  Make sure the working directory is set to the root of the repository.")
}
base::source(pathInstallPackages, local=new.env()) 

base::rm(pathInstallPackages)
###################################
# Load the necessary packages.
base::require(base)
base::require(knitr)
base::require(markdown)
base::require(testit)


#########################################################################################################
####
#### The following example comes from https://github.com/wibeasley/Wats.  Rename the paths appropriately.
####


###################################
# Declare the paths of the necessary files.

# The raw/input data files:
pathStoreLocationRaw <- "./Data/Raw/StoreLocation.csv"
pathDriveTimeTrafficRaw <- "./Data/Raw/Spatial/drive_time_traffic.json"
# pathCountyFips <- "./Datasets/CountyFipsCode.csv"
# 
# The derived/intermediate data files (which are produced by the repository's code files):
pathStoreLocationDerived <- "./Data/Derived/StoreLocation.rds"
pathDriveTimeTrafficDerived <- "./Data/Derived/Spatial/TrafficDataFrame.rds"
# 
# # Code Files:
pathGroomLocations <- "./Manipulate/GroomLocations.R"
pathPolygons <- "./Manipulate/GroomDriveTimePolygons.R"

# #Report Files:
# pathsReports <- base::file.path("./vignettes", c("MbrFigures.Rmd", "OkFertilityWithIntercensalEstimates.Rmd"))
# 
###################################
# Verify the necessary path can be found.

# The raw/input data files:
testit::assert("Points of existing Lenta stores should exist.", base::file.exists(pathStoreLocationRaw))
testit::assert("Polygons of the heavy traffic drive times should exist.", base::file.exists(pathDriveTimeTrafficRaw))

# Code Files:
testit::assert("The file that grooms the store locations data should exist.", base::file.exists(pathGroomLocations))
testit::assert("The file that calculates the GFR should exist.", base::file.exists(pathPolygons))
# 
# #Report Files:
# testit::assert("The knitr Rmd files should exist.", base::file.exists(pathsReports))
# 
###################################
# Run the files that manipulate and analyze.

# Execute code that grooms the datasets
base::source(pathGroomLocations, local=base::new.env())
base::source(pathPolygons, local=base::new.env())
# 
# # Assert that the intermediate files exist (the two files produced by `IsolateCensusPopsForGfr.R`)
# testit::assert("The yearly records should exist.", base::file.exists(pathCensusYearly))
# testit::assert("The monthly records should exist.", base::file.exists(pathCensusMonthly))
# 
# #Execute code that combines the census and birth count data.
# base::source(pathCalculateGfr, local=base::new.env())
# 
# # Verify that the two human readable datasets are present.
# testit::assert("The CSV for the 2005 Version should exist.", base::file.exists(pathDataForAnalaysis2005))
# testit::assert("The CSV for the 2014 Version should exist.", base::file.exists(pathDataForAnalaysis2014))
# 
# ###################################
# # Build the reports
# for( pathRmd in pathsReports ) {
#   pathMd <- base::gsub(pattern=".Rmd$", replacement=".md", x=pathRmd)
#   pathHtml <- base::gsub(pattern=".Rmd$", replacement=".html", x=pathRmd)
#   knitr::knit(input=pathRmd, output=pathMd)
#   markdown::markdownToHTML(file=pathMd, output=pathHtml)
# }
