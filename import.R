## Script to import the FIA data

##
## Load package ##
##

library(data.table)

##
## Tree table ####
##

## Select columns of interest (fasten the downloading)
col.keep.tree <- c(
  'CN',
  'PLT_CN',
  'SPCD',
  'SUBP',
  'DIA',
  'TPA_UNADJ',
  'STATUSCD')

## Define column classes
colclass.tree <- c(
  'CN' = "character",
  'PLT_CN' = "character",
  'SUBP'= "character",
  'SPCD' = "factor",
  'DIA' = "numeric",
  'TPA_UNADJ' =  "numeric",
  'STATUSCD' =  "factor")

## Link to download the data
path.tree <- "https://apps.fs.usda.gov/fia/datamart/CSV/TREE.csv" # From the website, may be long.

## Import the table
usa_tree <- fread(path.tree, 
                  header=TRUE, 
                  select=col.keep.tree, 
                  colClasses = colclass.tree,
                  verbose=TRUE)

##
## Plot table ####
##

## Select columns of interest
col.keep.plot <- c(
  'CN',
  'PLOT',
  'PREV_PLT_CN',
  'REMPER',
  'ELEV',
  'MEASYEAR',
  'INVYR',
  'LAT',
  'LON',
  'ECOSUBCD',
  'KINDCD',
  'DESIGNCD',
  'QA_STATUS',
  'MANUAL',
  'SAMP_METHOD_CD',
  'STATECD',
  'UNITCD',
  'COUNTYCD') 

## Define column classes
colclass.plot <- c(
  'CN' = "character",
  'PLOT' = "character",
  'PREV_PLT_CN' = "character",
  'REMPER'  = "numeric",
  'ELEV' = "numeric",
  'MEASYEAR' = "integer",
  'INVYR' = "integer",
  'LAT' = "numeric",
  'LON' = "numeric",
  'ECOSUBCD' = "character",
  'KINDCD' = "character",
  'DESIGNCD' = "character",
  'QA_STATUS' = "character",
  'MANUAL' = "character",
  'SAMP_METHOD_CD' = "character",
  'STATECD' = "character",
  'UNITCD' = "character",
  'COUNTYCD' = "character")

## Link to download the data
path.plot <- "https://apps.fs.usda.gov/fia/datamart/CSV/PLOT.csv"

## Import the table
usa_plot <- fread(path.plot,
                  header=TRUE, 
                  select=col.keep.plot, 
                  colClasses = colclass.plot,
                  verbose=TRUE)

##
## Subplot table ####
##

## Select columns of interest
col.keep.subplot <- c(
  'CN',
  'SUBP',
  'PLOT',
  'STATECD',
  'UNITCD',
  'COUNTYCD',
  'PREV_SBP_CN',
  'INVYR',
  'SUBP_STATUS_CD',
  'SUBPCOND')

## Define column classes
colclass.subplot <- c(
  'CN' = "character",
  'SUBP' = "character",
  'PLOT' = "character",
  'STATECD' = "character",
  'UNITCD' = "character",
  'COUNTYCD' = "character",
  'PREV_SBP_CN' = "character",
  'INVYR' = "integer",
  'SUBP_STATUS_CD' = "character",
  'SUBPCOND' = "character")

## Path to download the data
path.subplot <- "https://apps.fs.usda.gov/fia/datamart/CSV/SUBPLOT.csv"

## Import the table
usa_subplot <- fread(path.subplot,
                     header=TRUE, 
                     select=col.keep.subplot, 
                     colClasses = colclass.subplot,
                     verbose=TRUE)

##
## Condition table ####
##

## Select columns of interest
col.keep.cond <- c(
  'CN',
  'PLT_CN',
  'PLOT',
  'COND_STATUS_CD',
  'CONDPROP_UNADJ',
  'SUBPPROP_UNADJ',
  'STDORGCD',
  'DSTRBCD1',
  'AFFORESTATION_CD',
  'PREV_AFFORESTATION_CD',
  'LAND_COVER_CLASS_CD',
  'TRTCD1',
  'PHYSCLCD',
  'STATECD',
  'UNITCD',
  'COUNTYCD',
  'ASPECT',
  'SLOPE',
  'DSTRBCD2',
  'DSTRBCD3',
  'TRTCD2',
  'TRTCD3',
  'PRESNFCD') 

## Define column classes
colclass.cond <- c(
  'CN' = "character",
  'PLT_CN' = "character",
  'PLOT' = "character",
  'COND_STATUS_CD' = "character",
  'CONDPROP_UNADJ' = "numeric",
  'SUBPPROP_UNADJ' = "numeric",
  'STDORGCD' = "character",
  'DSTRBCD1' = "character",
  'AFFORESTATION_CD' = "character",
  'PREV_AFFORESTATION_CD' = "character",
  'LAND_COVER_CLASS_CD' = "character",
  'TRTCD1' = "character",
  'PHYSCLCD' = "character",
  'STATECD' = "character",
  'UNITCD' = "character",
  'COUNTYCD' = "character",
  'ASPECT' = "integer",
  'SLOPE' = "integer",
  'DSTRBCD2' = "character",
  'DSTRBCD3' = "character",
  'TRTCD2' = "character",
  'TRTCD3' = "character",
  'PRESNFCD' = "character")

## Path to download the data
path.cond <- "https://apps.fs.usda.gov/fia/datamart/CSV/COND.csv"

## Import the table
usa_cond <- fread(path.cond,
                  header=TRUE, 
                  select=col.keep.cond, 
                  colClasses = colclass.cond,
                  verbose=TRUE)

## Another option is to download all the FIA data: https://apps.fs.usda.gov/fia/datamart/CSV/ENTIRE.zip and save it locally
## To only download the TREE table: https://apps.fs.usda.gov/fia/datamart/CSV/TREE.zip
