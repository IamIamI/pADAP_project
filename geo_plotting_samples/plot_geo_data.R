# A random function found on stackoverflow to remove all currently loaded pacakges... need this to prevent plyr/dplyr conflicts
detachAllPackages <- function() {
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
}

detachAllPackages()

#Function to check if package is already installed, if not, installs it. After install it and loads it
install_load <- function(Required_Packages) {
  for(package in Required_Packages){
    if (!package %in% installed.packages()) install.packages(package, character.only = TRUE)
    library(package, character.only = TRUE)
  }
}

install_load(c("openxlsx","plyr"))


#Read in all the excell file will all sequenced samples, remove duplicates, subset only the region and countries
table_input <- read.xlsx("Sequenced_samples.xlsx", colNames = TRUE,
                         rowNames = FALSE, na.strings=c("","NA"))
table_input <- table_input[complete.cases(table_input[ , 2]),]
sumReseq <- table_input$Strain.ID[duplicated(table_input$Strain.ID)]
nonredundant_table_input <- table_input[!c(table_input$Strain.ID %in% sumReseq & grepl('Illumina', table_input$Library.Type)),]
sumSamples <- count(nonredundant_table_input, var=c("Region", "Country")) 
sumSamples$Region <- lapply(sumSamples$Region, function(x) paste(x, " Region"))
table_input[c(1,2,3,4),c(1,5,6,7,8)]
#Now detach plyr as it will conflict with dplyr afterwards
detachAllPackages()


install_load(c("ggplot2","colorspace","tmaptools","dplyr","maps","tmap","sf","sp"))

#Get geo data for regional counsils
#Get geo data for water boarder (gives the map a nice bevel)
nzgeo_file <- "nzgeo/REGC2017_GV_Clipped.shp"
#Check if the files actually exists, although this is not really functional, more for my debugging purposes
if (file.exists(nzgeo_file)){
  print("File accepted")
  nzgeo <- read_shape(file=nzgeo_file, as.sf = TRUE)
}

# For some weird reason the chatham in the water boarder shapefile is located just over the 180 degree line
# Therefore NZ spans the entire world according to this set... so just removing chathams will solve the problem
nzgeo_centroids <- st_centroid(nzgeo)


#Obtain city/population data for NZ, remove some redundant area's that all belong to the Auckland area
#city_data <- world.cities %>% filter(country.etc=="New Zealand")
#city_data$pop[city_data$name=="Auckland"] <- sum(city_data$pop[city_data$name=="Auckland"],
#                                                 city_data$pop[city_data$name=="Manukau"],
#                                                 city_data$pop[city_data$name=="North Shore"],
#                                                 city_data$pop[city_data$name=="Waitakere"])
#city_data <- city_data[!city_data$name=="Manukau" & !city_data$name=="North Shore" & !city_data$name=="Waitakere",]
#newdata <- city_data[order(city_data$pop, decreasing = TRUE),] 
#newdata_all.SP <- st_as_sf(newdata, coords = c("long", "lat"), crs = 4326)  
#newdata <- newdata[seq(from=1,to=8,by=1),]
#newdata_subset.SP <- st_as_sf(newdata, coords = c("long", "lat"), crs = 4326)  



frgeo_file <- "frgeo/regions-20170102.shp"
argeo_file <- "argeo/ArgentinaBoundary34.shp"
#Check if the files actually exists, although this is not really functional, more for my debugging purposes
if (file.exists(frgeo_file)){
  print("File accepted")
  frgeo <- read_shape(file=frgeo_file, as.sf = TRUE)
}

if (file.exists(argeo_file)){
  print("File accepted")
  argeo <- read_shape(file=argeo_file, as.sf = TRUE)
}
frgeo2 <- frgeo[frgeo$insee!="03",] 
frgeo2 <- frgeo2[frgeo2$insee!="01",] 
frgeo2 <- frgeo2[frgeo2$insee!="02",] 
frgeo2 <- frgeo2[frgeo2$insee!="04",] 
frgeo2 <- frgeo2[frgeo2$insee!="06",] 
frgeo_centroids <- st_centroid(frgeo2)
argeo_centroids <- st_centroid(argeo)

#FUCK IT.... i can't merge sumSample with nzgeo_centroids... so i'll just copy paste manually... fucking R
nzgeo_centroids$Samples <- c(0,0,10,0,0,3,0,10,0,5,33,2,5,0,2,0,3)
frgeo_centroids$Samples <- c(0,0,0,0,0,0,1,0,0,2,0,0,0)
argeo_centroids$Samples <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1)

# Make the three country maps
newzealand_shape_plot <- tm_shape(nzgeo) +
  tm_layout(bg.color="#f4f3ff", 
            frame="#d9d1ff", 
            frame.lwd=3, 
            main.title="Sample distribution",
            asp = 2) +
  tm_fill("#b2d8c5") +
  tm_borders(col = "#0c94a7", 
             lwd = 1, 
             lty = "solid", 
             alpha = 0.4)+
  tm_shape(nzgeo_centroids) +
  tm_symbols(col = "#d726f1",
             size = "Samples",
             scale = 1.4,
             title.size="Samples per region",
             border.lwd=NA)

france_shape_plot  <-   tm_shape(frgeo2) +
  tm_layout(bg.color="#f4f3ff", 
            frame="#d9d1ff", 
            frame.lwd=3,
            asp = 1) +
  tm_fill("#b2d8c5") +
  tm_borders(col = "#0c94a7", 
             lwd = 1, 
             lty = "solid", 
             alpha = 0.4)+
  tm_shape(frgeo_centroids) +
  tm_symbols(col = "#d726f1",
             size = "Samples",
             scale = 0.6,
             legend.size.show = FALSE,
             border.lwd=NA) 


# Arrange the maps as if a monkey did it because tmap package doesn't allow proper paneling
tmap_arrange(newzealand_shape_plot, france_shape_plot, nrow=1,ncol=2)
