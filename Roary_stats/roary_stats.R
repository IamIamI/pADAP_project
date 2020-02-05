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

#Install and load the following packages
install_load(c("gplots","RColorBrewer","readxl","colorspace","dplyr","tcltk"))


#Gives a file prompt for the gene_absence_presence file
table_input <- read.csv("gene_presence_absence2.csv", sep=",", na.strings=c("","NA"))


# Convert CSV file to a binary matrix, remove columns that are of no importance
table_input <- as.data.frame(table_input)
table_values <- within(table_input, rm("Gene","Non.unique.Gene.name","No..isolates","No..sequences","Avg.sequences.per.isolate","Genome.Fragment","Order.within.Fragment","Accessory.Fragment","Accessory.Order.with.Fragment","QC","Min.group.size.nuc","Max.group.size.nuc","Avg.group.size.nuc"))

#Convert to binary matrix, 1 meansing present, 0 meaning absent
abscence_presence <- as.matrix(table_values[,-1])
rownames(abscence_presence) <- table_values[,1]
abscence_presence[is.na(abscence_presence)] <- 0
abscence_presence[which(abscence_presence!=0)] <- 1
class(abscence_presence) <- "numeric"
storage.mode(abscence_presence) <- "numeric"

binary_matrix <- mapply(abscence_presence, FUN=as.numeric)
binary_matrix <- matrix(data=binary_matrix, ncol=length(colnames(abscence_presence)), nrow=length(row.names(abscence_presence)))
row.names(binary_matrix) <- row.names(abscence_presence)
colnames(binary_matrix) <- colnames(abscence_presence)

# Save the heatmap to a file of your choosing
fileName <- tclvalue(tkgetSaveFile()) 
if(!grepl(".pdf", fileName)){
  fileName <- paste(c(fileName,"pdf"), collapse=".")
}
#You can change the size of the pdf here... thought it was too much waste of time to make this interactive too
pdf(fileName, 12, (length(rownames(binary_matrix))/40))
heatmap.2(binary_matrix, col = c("#FFE986","#FF736E"), main = "Absence/Presence of genes", trace="none", labRow=FALSE)
# Save the heatmap to a file of your choosing
dev.off()

#Or you can use the following if you do want y-labels
fileName <- tclvalue(tkgetSaveFile()) 
if(!grepl(".pdf", fileName)){
  fileName <- paste(c(fileName,"pdf"), collapse=".")
}
#You can change the size of the pdf here... thought it was too much waste of time to make this interactive too
pdf(fileName, 12, (length(rownames(binary_matrix))/10))
heatmap.2(binary_matrix, col = c("#FFE986","#FF736E"), main = "Absence/Presence of genes", trace="none")
dev.off()

#Calculate number of genomes, this is needed later
genomes_count <- length(colnames(abscence_presence))

#Add a sum of genomes in clusters (how many genomes have gene X)
abscence_presence <- cbind(abscence_presence, rowSums(abscence_presence))

#Make a new table that holds our dat of interest
summary_table <- matrix(data=NA, nrow=3, ncol=length(colnames(abscence_presence)))
colnames(summary_table) <- colnames(abscence_presence)
rownames(summary_table) <- c("Total_genes","Unique_genes","Core_genes")
#Sum up all genes per genome
summary_table[1,] <- colSums(abscence_presence)
#Sum up all "unique" genes per genome by subsetting the data for rows that only contain 1 gene in the orthologous cluster
summary_table[2,] <- colSums(abscence_presence[which(abscence_presence[,ncol(abscence_presence)] == 1),])
#Sum up all "CORE" genes per genome by subsetting the data for rows that contain 1 gene in the orthologous cluster
summary_table[3,] <- colSums(abscence_presence[which(abscence_presence[,ncol(abscence_presence)] >= (genomes_count*0.95)),])
#Remove last column as this is just a summary of the "rowSum" column in abscence_prescense
summary_table <- summary_table[,-ncol(summary_table)]


#Secondary table with some average info for all data processed by Roary
average_table <- data.frame(x=1:6, y=1:6, z=1:6)
average_table[,1] <- c("Total genes analyzed","Orthologous groups","Average gene count","Average core genes","Average unique genes","Total unique genes")
average_table[1,2] <- sum(summary_table[1,])
average_table[2,2] <- length(rownames(abscence_presence))
average_table[3,2] <- median(summary_table[1,])
average_table[4,2] <- length(rownames(abscence_presence[which(abscence_presence[,ncol(abscence_presence)] >= (genomes_count*0.95)),]))
average_table[5,2] <- round(length(rownames(abscence_presence[which(abscence_presence[,ncol(abscence_presence)] == 1),]))/length(colnames(abscence_presence)))
average_table[6,2] <- length(rownames(abscence_presence[which(abscence_presence[,ncol(abscence_presence)] == 1),]))


#We have to melt the data for ggplot to be able to process it properly
#And we will use gridExtra and grid to plot all graphs into one file
install_load(c("ggplot2","gridExtra","grid","reshape2"))
melt_summary_table <- melt(summary_table)
melt_summary_table <- melt_summary_table[order(melt_summary_table$value),]

#Get a filename for the second plot
fileName <- tclvalue(tkgetSaveFile()) 
if(!grepl(".pdf", fileName)){
  fileName <- paste(c(fileName,"pdf"), collapse=".")
}
#You can change the size of the pdf here... thought it was too much waste of time to make this interactive too
pdf(fileName, 10, 7)
#First plot is the plot for total_genes per genome, total unique genes per genome and total "core" genes per genome
#It's a flipped bar chart, if you want anything else just change geom_bar into geom_line or geom_anythingelse
p1 <- ggplot(melt_summary_table, aes(x = reorder(Var2, value), y = value)) + 
          geom_bar( stat = 'identity') + 
          facet_grid(. ~ Var1, scales = "free_x") + 
          xlab("Genomes") +
          ylab("Count") +
          coord_flip()
#Second plot is for the "averages" so it shows average number of genes, average number of unique genes, and average number of core genes
#Could be handy for outliers, shows how much they deviate from the norm
p2 <- ggplot(data=average_table[-c(1,2,6),], aes(x=x, y=y))+
          geom_bar(stat = 'identity') +
          theme (axis.text.x=element_text(angle=90,hjust=1,vjust=0.3),
          axis.title.x = element_blank()) +
          geom_text(aes(y = 10, label = paste("N =" ,y),vjust = 0), colour = "white", show.legend=FALSE) +
          ylab("Count")
#And a little text box saying the following
t1  <- textGrob(paste(c("Total number of genomes:\n",
                        length(colnames(summary_table)),
                        "\n\nNumber of analyzed genes:\n",
                        as.numeric(average_table[1,2]),
                        "\n\nTotal orthologous groups\n",
                        as.numeric(average_table[2,2]),
                        "\n\nTotal unique genes\n",
                        as.numeric(average_table[6,2])), collapse = " ")) 
#This is the matrix that the figures will be plotted in, p1 taking up 8 cells, and P2 and t1 only 2 each
lay <- rbind(c(1,1,2),
             c(1,1,2),
             c(1,1,3),
             c(1,1,3))
#Saves pdf
grid.arrange(p1,p2,t1, layout_matrix = lay)

#Thank you for flying Lesley Airlines!
dev.off()
