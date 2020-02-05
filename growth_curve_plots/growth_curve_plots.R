#A random function found on stackoverflow to remove all currently loaded pacakges... need this to prevent plyr/dplyr conflicts
detachAllPackages <- function() {
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
}
detachAllPackages()

#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

#Function to check if package is already installed, if not, installs it. After install it and loads it
install_load <- function(Required_Packages) {
  for(package in Required_Packages){
    if (!package %in% installed.packages()) install.packages(package, character.only = TRUE)
    library(package, character.only = TRUE)
  }
}

#Install and load the following packages
install_load(c("ggplot2","tidyr","reshape","gridExtra"))

file_extention1 <- file.choose()
file_extention2 <- file.choose()
file_extention3 <- file.choose()


#Gives a file prompt for the gene_absence_presence file
table_input <- read.csv(file_extention1, sep=",", na.strings=c("","NA"))
colnames(table_input)<-table_input[1,]
table_input <- table_input[-1,]
table_input[,1] <- c("A1MO2 r1","A1MO2 r2","A1MO2 r3","A1MO2 r4",
                     "5.6 r1","5.6 r2","5.6 r3","5.6 r4",
                     "A1MO2 average",
                     "5.6 average")

datn2 <- melt(table_input, id='11')
colnames(datn2) <- c("Sample","Time","OD")
datn2$Time <- as.numeric(levels(datn2$Time)[datn2$Time])

#datn2$variable <- factor(datn2$variable)
p1 <- ggplot(data=datn2, aes(x=Time, y=OD, colour=Sample)) +
      geom_point(data=subset(datn2, datn2$Sample %in% c("A1MO2 r1","A1MO2 r2","A1MO2 r3","A1MO2 r4")), alpha=0.13, size=0.9)+
      geom_point(data=subset(datn2, datn2$Sample %in% c("5.6 r1","5.6 r2","5.6 r3","5.6 r4")), alpha=0.13, size=0.9)+
#     geom_point(data=subset(datn2, datn2$Sample %in% c("A1MO2 average","5.6 average"))) +
#     geom_line(data=subset(datn2, datn2$Sample %in% c("A1MO2 r1","A1MO2 r2","A1MO2 r3","A1MO2 r4")), alpha=0.2)+
#     geom_line(data=subset(datn2, datn2$Sample %in% c("5.6 r1","5.6 r2","5.6 r3","5.6 r4")), alpha=0.2)+
      geom_line(data=subset(datn2, datn2$Sample %in% c("A1MO2 average","5.6 average")),size=1.2) +
      ggtitle("Growth in LB broth") +
      theme(legend.position="none")

#Gives a file prompt for the gene_absence_presence file

table_input <- read.csv(file_extention2, sep=",", na.strings=c("","NA"))
colnames(table_input)<-table_input[1,]
table_input <- table_input[-1,]
table_input[,1] <- c("A1MO2 r1","A1MO2 r2","A1MO2 r3","A1MO2 r4",
                     "5.6 r1","5.6 r2","5.6 r3","5.6 r4",
                     "A1MO2 average",
                     "5.6 average")

datn2 <- melt(table_input, id='11')
colnames(datn2) <- c("Sample","Time","OD")
datn2$Time <- as.numeric(levels(datn2$Time)[datn2$Time])

#datn2$variable <- factor(datn2$variable)
p3 <- ggplot(data=datn2, aes(x=Time, y=OD, colour=Sample)) +
      geom_point(data=subset(datn2, datn2$Sample %in% c("A1MO2 r1","A1MO2 r2","A1MO2 r3","A1MO2 r4")), alpha=0.13, size=0.9)+
      geom_point(data=subset(datn2, datn2$Sample %in% c("5.6 r1","5.6 r2","5.6 r3","5.6 r4")), alpha=0.13, size=0.9)+
      #     geom_point(data=subset(datn2, datn2$Sample %in% c("A1MO2 average","5.6 average"))) +
      #     geom_line(data=subset(datn2, datn2$Sample %in% c("A1MO2 r1","A1MO2 r2","A1MO2 r3","A1MO2 r4")), alpha=0.2)+
      #     geom_line(data=subset(datn2, datn2$Sample %in% c("5.6 r1","5.6 r2","5.6 r3","5.6 r4")), alpha=0.2)+
      geom_line(data=subset(datn2, datn2$Sample %in% c("A1MO2 average","5.6 average")),size=1.2) +
      ggtitle("Growth in LB broth + 0.5mg/ml MitC") +
      theme(legend.position="none")

#Gives a file prompt for the gene_absence_presence file

table_input <- read.csv(file_extention3, sep=",", na.strings=c("","NA"))
colnames(table_input)<-table_input[1,]
table_input <- table_input[-1,]
table_input[,1] <- c("A1MO2 r1","A1MO2 r2","A1MO2 r3","A1MO2 r4",
                     "5.6 r1","5.6 r2","5.6 r3","5.6 r4",
                     "A1MO2 average",
                     "5.6 average")

datn2 <- melt(table_input, id='11')
colnames(datn2) <- c("Sample","Time","OD")
datn2$Time <- as.numeric(levels(datn2$Time)[datn2$Time])

#datn2$variable <- factor(datn2$variable)
p2 <- ggplot(data=datn2, aes(x=Time, y=OD, colour=Sample)) +
      geom_point(data=subset(datn2, datn2$Sample %in% c("A1MO2 r1","A1MO2 r2","A1MO2 r3","A1MO2 r4")), alpha=0.13, size=0.9)+
      geom_point(data=subset(datn2, datn2$Sample %in% c("5.6 r1","5.6 r2","5.6 r3","5.6 r4")), alpha=0.13, size=0.9)+
      #     geom_point(data=subset(datn2, datn2$Sample %in% c("A1MO2 average","5.6 average"))) +
      #     geom_line(data=subset(datn2, datn2$Sample %in% c("A1MO2 r1","A1MO2 r2","A1MO2 r3","A1MO2 r4")), alpha=0.2)+
      #     geom_line(data=subset(datn2, datn2$Sample %in% c("5.6 r1","5.6 r2","5.6 r3","5.6 r4")), alpha=0.2)+
      geom_line(data=subset(datn2, datn2$Sample %in% c("A1MO2 average","5.6 average")),size=1.2) +
      ggtitle("Growth in M9 Minimal media") +
      theme(legend.position="none")

p4 <- ggplot(data=datn2, aes(x=Time, y=OD, colour=Sample)) +
      geom_line() +
      geom_point()

mylegend<-g_legend(p4)

p5 <- grid.arrange(p1, p2,
                   p3, mylegend,
                   ncol = 4, nrow = 2,
                   layout_matrix = rbind(c(1,1,2,2), c(3,3,NA,4)))
