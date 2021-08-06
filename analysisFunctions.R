###################################################
###################################################
# Tasks:
# 1)Calculate home range sizes of each group
# 2)Calculate home range overlaps between groups
# 3)Calculate total area covered by all groups
# 4)Calculate the daily travel distance of each gorilla group 
# (time series analysis: Get result by time) - groupby() in pandas
###################################################
###################################################


#### >>>>>>>>>>>PLAN>>>>>>>>>>>>>>>>>.
#
# Develop a R package for home range estimation analysis with simple csv input
# Develop flexdashboard for interactive display of results
# Story map and other visualization
# 
# Functions include:
# csv_require(): help function to understand how to input csv file
# GPS2df(): robustly convert traditional field data format (csv) into data frame
# getSHP(): export shape file from the spatial polygon data frame
# mapDisplay(): display home ranges of groups in an interactive map
# areaBar(): bar chart of home range size
# mcpAnalysis(): mcp analysis to understand density distribution of groups
# HRstat(): display key statistics about gorilla groups regarding their home range
# overlapBar(): bar chart displaying overlapping home range of Gorilla groups
# HRcorr(): scatter plot showing home range size correlation with their share of home range
# GPheatmap(): heatmap with clustering to visualize home range dynamics between groups

####
#TO DO
####

# 1) Filter outliner points - out of national park boundary (polygon)
# 2) Analysis by day/ month/ year

#### =========SETTING UP ENVIRONMENT============= ####

# Set up working directory
getwd() #current directory
setwd("C:/Users/Admin/Desktop/Intern_HiWi/GorillaFund") #new directory
options(warn=-1) #suppress all warnings

# Import libraries
library(sp) # basic package for spatial analysis and plotting
library(raster) # working with gridded data
library(RColorBrewer) # for customized color palette
library(data.table) # data table format
library(leaflet) # interactive visualization
library(ggplot2) # general plotting
library(mapview) # interactive visualization
library(dbplyr) # data manipulation
library(rgdal) # useful for writing shapefile
library(rgeos)
library(maptools)
library(tidyverse)
library(scales)
library(sf) # for spatial analysis
library(lattice)
library(leafpop)
#library(vapoRwave)
library(viridis)
library(units)
library(adehabitatHR) # package for home range calculation

#### Set up plotting environment
while (!is.null(dev.list()))  dev.off() #turn off device if there is
#par(mar=c(1,1,1,1)) #reset margin value
#par("mar") #check margin value

# csv_require() function explains general requirement for input to all analysis functions
# It is used as an instruction help before using GPS2df() to read data.
# This function does not have argument.

csv_require <- function() {
  print("1) GPS2df() function can only read csv file, not a xlsx file.")
  print("2) The csv file need to have 6 columns,including the keywords: 'Longitude','Latitude','Altitude','Date','GORILLA_GROUP','LABEL'")
  print("3) The column names are case insensitive.")
  print("4) The default mode will read data column in the format of '%d-%b-%y',if it is in another format, it can be specified in the dateFormat argument.")
  print("5) The default mode will take the full period provided ")
}
# printing out basic requirements for inputting a csv file

# GPS2df() is the main function for reading csv file into a dataframe for further analysis.
# It parses time steps and can be used to filter data range. 
# The output data.frame is required for all other functions.
# dateFormat defines the format for the dates in the csv file. 
# start indicates the beginning data for analysis (default value is NULL)
# end indicates the ending data for analysis (default value is NULL)

GPS2df <- function(csv,dateFormat = "%d-%b-%y",start = NULL, end = NULL) {
  if (file.exists(csv) == 0) { # check if there input file exists in the current directory
    stop("File cannot be found.") # display error
  }
  if (grep(".csv", csv, fixed=T) != TRUE){ # check if the input is a csv file
    stop("Please input a .csv file instead") # display warning
  }
  read <- read.csv(csv,  # read csv file
                   header = TRUE, check.names = TRUE, 
                   stringsAsFactors = FALSE)
  
  name <- names(read) # extract the names of columns in a list
  
  # from the list, grep the columns needed independent of order and case
  col_x <- grep("Longitude",name,ignore.case=TRUE,value=FALSE) #x
  col_y <- grep("Latitude",name,ignore.case=TRUE,value=FALSE) #y
  col_z <- grep("Altitude",name,ignore.case=TRUE,value=FALSE) #z
  col_t <- grep("Date",name,ignore.case=TRUE,value=FALSE) #time stamps
  col_id <- grep("GORILLA_GROUP",name,ignore.case=TRUE,value=FALSE) #group
  col_lab <- grep("LABEL",name,ignore.case=TRUE,value=FALSE) #additional information
  
  if (length(col_x) != 1) { # catch errors if column name not available
    stop("Column 'Longitude' cannot be found")
  }
  if (length(col_y) != 1) {
    stop("Column 'LABEL' cannot be found")
  }
  if (length(col_z) != 1) {
    stop("Column 'LABEL' cannot be found")
  }
  if (length(col_t) != 1) {
    stop("Column 'Date' cannot be found")
  }
  if (length(col_id) != 1) {
    stop("Column 'GORILLA_GROUP' cannot be found")
  }
  if (length(col_lab) != 1) {
    stop("Column 'LABEL' cannot be found")
  }
  
  list <- c(col_x,col_y,col_z,col_t,col_id,col_lab) # list of columns to use
  
  read_sub <- read[ ,list] # read the columns
  
  colnames(read_sub) <- c("x", "y", "z", "date", "id", "label") # rename columns
  
  # Remove rows with NA's
  read_sub <- read_sub[!is.na(read_sub$y) & !is.na(read_sub$x),] # remove missing rows
  
  # Parse Data Time
  read_sub$date <- as.Date(read_sub$date, dateFormat) # parse the date into a datatime format for time series analysis
  
  if (class(read_sub$date) != "Date") { # check if reading data time is successful
    print("The input date format is invalid.")
  }
  
  # Sort Date
  read_sub <- read_sub[order(read_sub$date),] # sort date time in order
  
  # Filter Date
  if (is.null(start) != 1 & is.null(end) != 1) {
    read_sub <- read_sub[start:end,] # take the full data range if start and end is not provided
  } else if (is.null(start) == 1 & is.null(end) != 1) { # count on start only if end not provided
    read_sub <- read_sub[min(read_sub$date):end,]
  } else if (is.null(start) != 1 & is.null(end) == 1) { # count on end only if start not provided
    read_sub <- read_sub[start:max(read_sub$date),]
  } else {
  }  
  return(read_sub) # return rows within the date range
}

# ttlHR() is a function for calculating the total home range areas for all groups.
# Overlap areas will be only count once. 
# The output is a float number with the unit defined by areaUnit.
# crs argument is the string of coordinate reference system (in the formate of "+init=epsg:" followed by epsg)
# par mean the percentage of probability distribution being count as home range. 
# The default par is 95 (%)
# areaUnit indicates the unit for home range area: 
# it can be either "m2" for square metre or "km2" for square kilometre

ttlHR <- function(df,crs="+init=epsg:32735",par=95,areaUnit="km2") {
  # Total Ranging Area - All Gorilla Groups
  df.sp <- df[, c("x", "y")] # take x and y
  coordinates(df.sp) <- c("x", "y") # set x & y as coordinates
  
  proj4string(df.sp) <- CRS(crs) # set up projection CRS
  
  # HREF
  kernel.ref <- kernelUD(df.sp, h = "href")  # href = the reference bandwidth
  
  # Home range size
  # Get polygons (with attributes)
  df.kernel.poly <- getverticeshr(kernel.ref, percent = par, unout = areaUnit)# get spatial polygons
  return(df.kernel.poly$area) # get area of the polygons
}

# gpHR() is a function for calculating home range size for every Gorilla group,
# specifised with the Gorilla Group column in the csv.
# Input argument is the data.frame created from GPS2df().
# crs argument is the string of coordinate reference system (in the formate of "+init=epsg:" followed by epsg)
# par mean the percentage of probability distribution being count as home range. 
# The default par is 95 (%)
# areaUnit indicates the unit for home range area: 
# it can be either "m2" for square metre or "km2" for square kilometre

gpHR <- function(df,crs="+init=epsg:32735",par=95,areaUnit="km2") {
  # Ranging Area - For Each Gorilla Group
  df.sp <- df[, c("id", "x", "y")] # take x, y, and id to group information by groups
  coordinates(df.sp) <- c("x", "y") # set x & y as coordinates
  
  proj4string(df.sp) <- CRS(crs) # set up projection CRS
  
  # HREF
  kernel.ref <- kernelUD(df.sp, h = "href")  # href = the reference bandwidth
  
  # Home range size
  # Get polygons (with attributes)
  df.kernel.poly <- getverticeshr(kernel.ref, percent = par, unout = areaUnit) # get spatial polygon
  
  # Form new data frame
  id <- c(df.kernel.poly$id) # extract attributes from the polygon
  area <- c(df.kernel.poly$area)
  
  df_group <- data.frame(id,area) # use attributes to form data frame
  
  return(df_group) # return data.frame
}

# areaBar() is a function for plotting home range size of the Gorilla groups 
# with a bar chart. Every group is represented by a different color.
# color argument is used to defined color palette for visualization.
# Input argument is the data.frame created from gpHR().
# Output is a ggplot figure.

areaBar <- function(df, color="RdYlGn") {
  fig <- ggplot(data = df, aes(x=id, y=area, fill=id)) + # ggplot 
    geom_bar(stat="identity", width=0.5) + # bar chart
    scale_fill_brewer(palette = color) +  #custom color
    xlab("Gorilla Group") +  #layout
    ylab("Home Range Size") + 
    ggtitle("Home Range Estimation", subtitle = waiver())
  
  return(fig) # return plot
}

# getSPDF() is a function for returning home range polygon as spatial polygon data.frame 
# The data.frame can be used for further statistical analysis.
# An important argument is by_group: 
# TRUE: The polygon is by Gorilla group so the number of polygon is depends on the number of groups
# FALSE: It will return only one polygon for all Gorilla groups.
# crs argument is the string of coordinate reference system (in the formate of "+init=epsg:" followed by epsg)
# par mean the percentage of probability distribution being count as home range. 
# The default par is 95 (%)
# areaUnit indicates the unit for home range area: 
# it can be either "m2" for square metre or "km2" for square kilometre

getSPDF <- function(df,crs="+init=epsg:32735",par=95,areaUnit="km2",by_group=TRUE) {
  if(by_group == TRUE) { # check if the data.frame is for total population or each group
    df.sp <- df[, c("x", "y", "id")] 
  } else {
    df.sp <- df[, c("x", "y")]
  }
  
  coordinates(df.sp) <- c("x", "y") # take x and y
  proj4string(df.sp) <- CRS(crs) # set up projection CRS
  
  # HREF
  kernel.ref <- kernelUD(df.sp, h = "href")  # href = the reference bandwidth
  # estimator/algorithms for probability density function
  
  # Home range size
  # Get polygons (with attributes)
  df.kernel.poly <- getverticeshr(kernel.ref, percent = par, unout = areaUnit) 
  return(df.kernel.poly)
}

# getSHP() is a function for returning home range polygon as shapefile 
# The downloaded shapefile can be exported for further statistical analysis.
# The input argument is the spatial polygon data.frame returned by getSPDF().
# The name argument needs to be a string (inside "") 
# beginning with "/" and ends with the file extension. 
# An example would be "/homerange.shp", which is also the default name.
# path argument is by default the current working directory.
# It indicates the path used for storing the downloaded file.

getSHP <- function(poly,name="/homerange.shp",path=getwd()) { # take name and the path for the shapefile
  dsn = paste0(path,name) # combine path and name of the file
  if(file.exists(dsn)==TRUE){ # check if the name of the file already exists 
    stop("File of same name exists. Overwrite is not allowed. Please change the name of the output file.")
  } # show error
  writeOGR(poly, dsn = dsn, layer = 'poly', driver = "ESRI Shapefile", overwrite_layer = FALSE) # write shapefile
}

# ttlOverlap() is used for calculating the total overlapping home range,
# which is shared by multiple Gorilla Groups.
# The input argument is the data.frame returned by the GPS2df().

ttlOverlap <- function(df) {
  # Total Area of Home Range
  fullHR <- getSPDF(df, by_group = TRUE) # get total home range
  sumArea <- sum(fullHR$area)
  
  ttlHR <- getSPDF(df, by_group = FALSE) # get home range covered by total population
  totalArea <- ttlHR$area
  
  totalOverlap <- sumArea - totalArea # get total overlap
  
  return(totalOverlap) # return number
}

# mapDisplay() is a visualization function, used to plotting spatial distribution
# of the Gorilla groups. The output is a interactive map showing point locations
# of the Gorillas as well as group home range polygons. 
# crs argument is the string of coordinate reference system (in the formate of "+init=epsg:" followed by epsg)
# par mean the percentage of probability distribution being count as home range. 
# The default par is 95 (%)
# areaUnit indicates the unit for home range area: 
# It can be either "m2" for square metre or "km2" for square kilometre
# It needs a shapefile of national boundary in the current working directory
# (rwa_adm0_2006_NISR_WGS1984_20181002.shp) in order to run.

#option without polygon

mapDisplay <- function(df,crs="+init=epsg:32735",par=95,areaUnit="km2") { # take data frame for visulization on map
  rwa <- read_sf("rwa_adm0_2006_NISR_WGS1984_20181002.shp")[0] # read shapefile
  spd <- sf::as_Spatial(st_geometry(rwa), IDs = as.character(1:nrow(rwa)))
  poly <- getSPDF(df,crs=crs,par=par,areaUnit=areaUnit) # get spatial polygon
  df.sp <- df[, c("id", "x", "y")] # get by group data frame
  coordinates(df.sp) <- c("x", "y")
  proj4string(df.sp) <- CRS(crs)
  
  map <- mapview(rwa, alpha = 0.2, map.types = 'Esri.WorldImagery', legend = FALSE, label = NULL) + # add up elements for the map
    mapview(df.sp, cex = 3, alpha = 0.1, label = df.sp$id) + 
    mapview(poly, col.regions = "burlywood",  # home range polygons
            alpha.regions = 0.5, legend = FALSE, label = poly$id)
  map@map %>% setView(29.49, -1.5, zoom = 15) # set view for the map
  return(map) # return output
}

# mcpAnalysis() is a function for plotting an analysis plot using
# Minimum Convex Polygon esitmator. It shows the density distribution of 
# the home range areas. Data.frame returned by GPS2df() is the only argument.

mcpAnalysis <- function(df,crs="+init=epsg:32735",par=95,areaUnit="km2"){ # take date frame
  poly <- getSPDF(df,crs=crs,par=par,areaUnit=areaUnit) # get spatial polygons
  df.sp <- df[, c("id", "x", "y")]
  coordinates(df.sp) <- c("x", "y") # take coordinates
  proj4string(df.sp) <- CRS(crs) # set up CRS
  mcp <- mcp.area(df.sp, percent = seq(0, 100, by = 10)) # define plot levels
  return(mcp)
}

# overlapAnalysis() is a function used for analysing how different Gorilla groups 
# interact. It store detailed informaion about overlapping home range area of 
# different groups. areaUnit argument is either "km" or "m", representing the unit
# for area calculation. It takes a dataframe from GPS2df() and returns a dataframe 
# including group1 (primary group), group2 (interacting group), 
# group1_area (home range size of the primary group), 
# overlapHR (the absolute overlapping area in the unit defined by areaUnit),
# and overlapPar (the percentage (0-100) of home range area shared with the interacting group2).

overlapAnalysis <- function(df, areaUnit="km"){
  groupHR_df <- getSPDF(df, by_group = TRUE)
  # Get intersection using raster package
  overlap <- raster::intersect(groupHR_df, groupHR_df)
  # Save the sizes of polygons as DataFrame
  area <- st_area(st_as_sf(overlap))# Get the areas
  
  if (areaUnit=="km"){ # set up units for data frame depends on areaUnit
    area_unit <- units::set_units(area, km^2)
  } else if (areaUnit=="m"){
    area_unit <- units::set_units(area, m^2) # Set units
  } else {
    stop("areaUnit can only be either km or m.") # catch error
  }
  
  overlapArea <- as.data.frame(area_unit) # Convert into Data Frame
  
  # Save the overlap groups as another Dataframe
  overlap.df <- as.data.frame(overlap)
  
  # Drop unneeded columns - the ordinary home range size
  drops <- c("area.2")
  overlap.df <- overlap.df[ , !(names(overlap.df) %in% drops)]
  
  # Merge the size DF and the group name DF
  merged <- merge(overlap.df, overlapArea, by=0)
  merged <- merged[ , -which(names(merged) %in% c("Row.names"))]
  
  # Rename the columns
  colnames(merged) <- c('group1','group1_area','group2','overlapHR')
  
  # Sort the rows and reset the row index
  merged <- merged[order(merged$group1),]
  rownames(merged) <- NULL
  
  merged <- merged %>%  # create a new column as overlapping percentage
    mutate(overlapPar = round(overlapHR / group1_area,2)*100)
  
  attributes(merged$overlapPar) <- NULL # remove attributes
  
  return(merged) # return output
}

# overlapBar() is a function returning a bar chart visualizing how much 
# is the home range of the Gorilla groups shared with the other groups.
# It takes a data.frame returned from GPS2df().

overlapBar <- function(df) { # take data frame
  merged = overlapAnalysis(df) # use overlapAnalysis() to get data frame about overlap information
  merged_minus <- merged[merged$group1 != merged$group2, ] # remove overlap with one self
  
  barChart <- ggplot(
    merged_minus, aes(factor(group1), overlapPar, fill = group2)) +  # plot data
    geom_bar(stat="identity", position = "dodge") +  # bar chart
    scale_fill_brewer(palette = "Spectral") +  # custom colors
    labs(fill = "Overlapping Group") +  # layout: legend
    xlab("Group") +  # x label
    ylab("Overlap Proportion (%)") +  # y label
    ggtitle("Home Range Overlap", subtitle = waiver()) # title
  
  return(barChart) # return output
}

# shareBar() is used to visualize total shares of home range of each Gorilla group.
# Overlap proportion can be larger than 100%, indicating its home range is shared
# with multiple groups which add up the proportion.
# It takes a data.frame returned from GPS2df() and returns a bar chart.

shareBar <- function(df) {
  merged <- overlapAnalysis(df) # get overlapping information as data frame
  merged_minus <- merged[merged$group1 != merged$group2, ]   
  shareDF <- aggregate(merged_minus$overlapPar, by=list(group=merged_minus$group1), FUN=sum) # aggregating overlapping information belongs to the same group 
  shareDF <- shareDF[order(shareDF$x),] # order data frame
  rownames(shareDF) <- NULL # remove row names
  
  shareChart <- ggplot(data=shareDF, aes(x=reorder(group, -x), y=x, fill=group)) + # plot overlapping info
    geom_bar(stat="identity", show.legend = FALSE) + 
    scale_fill_brewer(palette = "Spectral") +  # color
    xlab("Group") +  # layout
    ylab("Total Share (%)") + 
    ggtitle("Home Range Share to Other Groups", subtitle = waiver())
  
  return(shareChart)
}

# HRcorr() is a function used to plot the correlation between home range size of a group
# and its total share to other groups. The output is a scatter plot with a regression line.
# crs argument is the string of coordinate reference system (in the formate of "+init=epsg:" followed by epsg)
# par mean the percentage of probability distribution being count as home range. 
# The default par is 95 (%)
# areaUnit indicates the unit for home range area: 
# It can be either "m2" for square metre or "km2" for square kilometre

HRcorr <- function(df,crs="+init=epsg:32735",par=95,areaUnit="km2") {
  merged <- overlapAnalysis(df) # get overlapping info
  merged_minus <- merged[merged$group1 != merged$group2, ] # remove duplicated info
  shareDF <- aggregate(merged_minus$overlapPar, by=list(group=merged_minus$group1), FUN=sum) # get info by group
  shareDF <- shareDF[order(shareDF$group),]
  rownames(shareDF) <- NULL # return a data frame with home range shares
  
  poly <- getSPDF(df,by_group=TRUE,crs=crs,par=par,areaUnit=areaUnit) # get home range area of every group for further analysis
  poly_df <- data.frame(poly$id,poly$area)
  colnames(poly_df) <- c("group","area")
  poly_df <- poly_df[order(poly_df$group),] # return a data frame with home range area
  
  shareDF <- rename(shareDF, percent = x)
  cor_df <- merge(poly_df, shareDF, by="group") # merge two data frames
  cor_df <- cor_df[ , !(names(cor_df) %in% c("Row.names","group.y"))]
  
  corrPlot <- ggplot(cor_df, aes(x=area, y=percent)) +  # plot correlation
    geom_point() + # plot points
    geom_smooth(method=glm, se=FALSE, fullrange=TRUE) + # plot regression line
    xlab("Home Range Size") + # layout
    ylab("Total Share (%)") +
    ggtitle("Correlation: Home Range Size and Shares", subtitle = waiver())
  
  return(corrPlot)
}

# GPheatmap() is a function returning a heatmap displaying the interaction between 
# different Gorilla groups. A dendrogram can be displayed with dendrogram = TRUE.
# The more red an area, the closer are the two groups (in the x and y axis) regarding
# their home range share.
# It takes a data.frame returned from GPS2df() and returns a heatmap.

GPheatmap <- function(df,dendrogram=TRUE) { # take data frame
  merged <- overlapAnalysis(df) # get overlap info
  gpName <- unique(merged$group1) # get group name
  drops <- c("group1_area","overlapPar") # drop unneeded info
  merged_clean <- merged[ , !(names(merged) %in% drops)]
  attributes(merged_clean$overlapHR) <- NULL # remove attrivutes
  
  mat <- matrix(0, nrow = length(gpName), ncol = length(gpName)) # create a zero matrix
  merged_clean.mtx <- as.matrix(data.matrix(merged_clean))
  mat[merged_clean.mtx[,1:2] ] <- merged_clean.mtx[,3] # set up a matrix for heatmap  - reshape data frame
  
  mat_num <- apply(mat, 2 ,as.numeric) # convert it to numerical
  
  rownames(mat_num) <- gpName # define row and column names
  colnames(mat_num) <- gpName
  
  coul <- colorRampPalette(brewer.pal(8, "YlOrRd"))(10) # define custom color palette
  
  if (dendrogram == TRUE) {
    gpheatmap <- heatmap(mat_num, scale="column", col = coul, verbose = FALSE) # heatmap without dendrogram
  } else {
    gpheatmap <- heatmap(mat_num, Colv = NA, Rowv = NA, scale="column", col = coul, verbose = FALSE) # heatmap with dendrogram
  }
  
  return(gpheatmap)
}

# dynamicViz() is a function showing all groups in two dimensions:
# home range size and total share. It take a data.frame returned by GPS2df()
# and returns a scatter plot.
# crs argument is the string of coordinate reference system (in the formate of "+init=epsg:" followed by epsg)
# par mean the percentage of probability distribution being count as home range. 
# The default par is 95 (%)
# areaUnit indicates the unit for home range area: 
# It can be either "m2" for square metre or "km2" for square kilometre

dynamicViz <- function(df,crs="+init=epsg:32735",par=95,areaUnit="km2") {
  merged <- overlapAnalysis(df) # get overlap info
  merged_minus <- merged[merged$group1 != merged$group2, ] # remove duplicated info
  shareDF <- aggregate(merged_minus$overlapPar, by=list(group=merged_minus$group1), FUN=sum) # by group info
  shareDF <- shareDF[order(shareDF$group),]
  rownames(shareDF) <- NULL
  shareDF <- rename(shareDF, percent = x)
  
  poly <- getSPDF(df,by_group=TRUE,crs=crs,par=par,areaUnit=areaUnit) # data frame for home range area
  poly_df <- data.frame(poly$id,poly$area)
  colnames(poly_df) <- c("group","area")
  poly_df <- poly_df[order(poly_df$group),]
  
  cor_df <- merge(poly_df, shareDF, by="group") # merge two data frames
  cor_df <- cor_df[ , !(names(cor_df) %in% c("Row.names","group.y"))]
  
  scatterPlot <- ggplot(cor_df, aes(x=area, y=percent, color=group)) +  # scatter plot: total home range share percentage & area size
    geom_point(size=4.5,shape=17) +  # plot points
    xlab("Home Range Size") +  # layout
    ylab("Total Share (%)") + 
    ggtitle("Home Range Size and Share", subtitle = waiver())
  
  return(scatterPlot)
}

# HRstat() is a function returning a list of key statistics 
# from the home range analysis.
# It includes:
# 1) fullHR: The home range size of the total population
# 2) GPmax: The group having the maximum home range size
# 3) maxArea: The areas from the group with maximum home range size
# 4) GPmin: The group having the minimum home range size
# 5) minArea: The areas from the group with minimum home range size
# 6) maxShare: The group with maximum sharing area
# 7) maxShareArea: The areas shared by the maximum sharing group
# 8) minShare: The group with minimum sharing area
# 9) minShareArea: The areas shared by the minimum sharing group
# It takes a data.frame returned from GPS2df().

HRstat <- function(df) {
  fullHR <- getSPDF(df, by_group = TRUE) # get info for full home range
  sumArea <- sum(fullHR$area) # sum of home range
  ttlHR <- getSPDF(df, by_group = FALSE) # get data frame for total population home range
  fullArea <- ttlHR$area # get area
  dfArea <- data.frame(fullHR$id,fullHR$area) # form new data frame
  GPmax <- dfArea[order(dfArea$fullHR.area,decreasing=TRUE),][1,]$fullHR.id # group with max area
  maxArea <- dfArea[order(dfArea$fullHR.area,decreasing=TRUE),][1,]$fullHR.area # corresponding group
  GPmin <- dfArea[order(dfArea$fullHR.area,decreasing=FALSE),][1,]$fullHR.id # group with max area
  minArea <- dfArea[order(dfArea$fullHR.area,decreasing=FALSE),][1,]$fullHR.area # corresponding group
  
  merged <- overlapAnalysis(df) # prepare data frame for overlapping info
  merged_minus <- merged[merged$group1 != merged$group2, ]   
  shareDF <- aggregate(merged_minus$overlapPar, by=list(group=merged_minus$group1), FUN=sum)
  shareDF <- shareDF[order(shareDF$x),]
  rownames(shareDF) <- NULL
  maxShare <- shareDF[order(shareDF$x,decreasing=TRUE),][1,]$group # group of max share
  maxShareArea <- shareDF[order(shareDF$x,decreasing=TRUE),][1,]$x # corresponding share
  minShare <- shareDF[order(shareDF$x,decreasing=FALSE),][1,]$group # group of min share
  minShareArea <- shareDF[order(shareDF$x,decreasing=FALSE),][1,]$x # corresponding share
  
  stat <- vector(mode="list", length=9) # get stat vector
  names(stat) <- c(fullArea,GPmax,maxArea,GPmin,minArea,maxShare,maxShareArea,minShare,minShareArea) # name of list
  stat[[1]] <- fullArea # put key statistics into the list
  stat[[2]] <- GPmax
  stat[[3]] <- maxArea
  stat[[4]] <- GPmin
  stat[[5]] <- minArea
  stat[[6]] <- maxShare
  stat[[7]] <- maxShareArea
  stat[[8]] <- minShare
  stat[[9]] <- minShareArea
  
  return(stat) # return key statistics
}




