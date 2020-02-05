table_input1 <- read.table("CFU.csv",header=TRUE,sep=",", 
                           na.strings="NA",dec=".",strip.white=TRUE)
table_input2 <- read.table("Protein.csv",header=TRUE,sep=",",
                           na.strings="NA",dec=".",strip.white=TRUE)
table_input3 <- read.table("Plate_count.csv",header=TRUE, sep=",", 
                           na.strings="NA", dec=".",strip.white=TRUE)


table_input1 <- read.table("CFU_20170808.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
table_input2 <- read.table("Protein_time.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
table_input3 <- read.table("GrowthcurveMH96.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)


table_input1$Mean <- log10(table_input1$Mean)


new_table_input <- merge(table_input1, table_input2, by="Time", all=TRUE, incomparables=NULL)
new_table_input <- merge(new_table_input, table_input3, by="Time", all=TRUE, incomparables=NULL)


dev.off()
par(mar=c(5, 3.5, 4, 8))


ymax <- round(max(new_table_input$Mean.y, na.rm=TRUE), digits=1)# Y-max value
ymin <- -0.1# Y-min value
m <- barplot(new_table_input$Mean.y, xpd=FALSE, ylim=c(ymin,ymax), xlab="Time", ylab="", col="gray")
mtext(expression("Spectrometer absorption"), side=2, line=2, col="black")
axis(side=2, ylim=c(ymin,ymax), col="black", col.axis="black", at=seq(-0.1,0.7,0.1))
axis(side=1, at=m, labels=new_table_input$Time) # <-- plot the Y-axis for the boxplot with the time labels instead of actual x-axis coordinates, this is to prevent labels from missing (as not all data had samples at the same timepoints)
abline(h=0) # <- i added a horizontal line to determine the negative values, spectrometer corrects values to negatives based on blanks. 


par(mar=c(5, 3.5, 4, 8))
ymax <- round(max(new_table_input$Mean.y, na.rm=TRUE), digits=1)# Y-max value
ymin <- -0.1# Y-min value
m <- barplot(new_table_input$Mean.y, xpd=FALSE, ylim=c(ymin,ymax), xlab="Time", ylab="", col="gray")
mtext(expression("Spectrometer absorption"), side=2, line=2, col="black")
axis(side=2, ylim=c(ymin,ymax), col="black", col.axis="black", at=seq(-0.1,0.7,0.1))
axis(side=1, at=m, labels=new_table_input$Time) # <-- plot the Y-axis for the boxplot with the time labels instead of actual x-axis coordinates, this is to prevent labels from missing (as not all data had samples at the same timepoints)
abline(h=0) # <- i added a horizontal line to determine the negative values, spectrometer corrects values to negatives based on blanks. 
par(new=T) # <-- plot a transparant new window
ymax <- ceiling(max(new_table_input$Mean.x, na.rm=TRUE))# Y-max value
ymin <- floor(min(new_table_input$Mean.x, na.rm=TRUE))# Y-min value
plot(m, ylim=c(ymin,ymax), axes=F, type="n", xlab="", ylab="") # <- plot new empty window, with the new y-axis values
# Calculate where X-axis starts, ends, and where the individual points are
xlims <- par('usr')[1:2] # <-- get Where the window of the plot starts and ends
xoffset <- par('plt')[4] # <-- get the offeset between the window and the actual start and end of the x-axis
x_intervals <- ((xlims[2]-xoffset)-(xlims[1]+xoffset))/length(new_table_input$Mean.x) # <-- calculate the step size
x_start <- xlims[1]+xoffset+(0.5*x_intervals) # <-- calculate the true x = 0 point
x_end <- xlims[2]-xoffset+(0.5*x_intervals) # <-- calculate the true x=24 point
# Create the X-ticks
new_x_axis <- seq(x_start,x_end,x_intervals)
# Plot the line, add the lext to y-axis, plot y-axis
lines(x=head(new_x_axis, -1), y=new_table_input$Mean.x, type="b",pch=19,lwd=2, ylim=c(ymin,ymax), col="black") # <-- plot the line of data type 2
mtext(expression(-log[10](italic(cfu))), side=4, line=2.5, col="black")
axis(side=4, col="black", col.axis="black")


par(mar=c(5, 3.5, 4, 8))
ymax <- round(max(new_table_input$Mean.y, na.rm=TRUE), digits=1)# Y-max value
ymin <- -0.1# Y-min value
m <- barplot(new_table_input$Mean.y, xpd=FALSE, ylim=c(ymin,ymax), xlab="Time", ylab="", col="gray")
mtext(expression("Spectrometer absorption"), side=2, line=2, col="black")
axis(side=2, ylim=c(ymin,ymax), col="black", col.axis="black", at=seq(-0.1,0.7,0.1))
axis(side=1, at=m, labels=new_table_input$Time) # <-- plot the Y-axis for the boxplot with the time labels instead of actual x-axis coordinates, this is to prevent labels from missing (as not all data had samples at the same timepoints)
abline(h=0) # <- i added a horizontal line to determine the negative values, spectrometer corrects values to negatives based on blanks. 
par(new=T) # <-- plot a transparant new window
ymax <- ceiling(max(new_table_input$Mean.x, na.rm=TRUE))# Y-max value
ymin <- floor(min(new_table_input$Mean.x, na.rm=TRUE))# Y-min value
plot(m, ylim=c(ymin,ymax), axes=F, type="n", xlab="", ylab="") # <- plot new empty window, with the new y-axis values
# Calculate where X-axis starts, ends, and where the individual points are
xlims <- par('usr')[1:2] # <-- get Where the window of the plot starts and ends
xoffset <- par('plt')[4] # <-- get the offeset between the window and the actual start and end of the x-axis
x_intervals <- ((xlims[2]-xoffset)-(xlims[1]+xoffset))/length(new_table_input$Mean.x) # <-- calculate the step size
x_start <- xlims[1]+xoffset+(0.5*x_intervals) # <-- calculate the true x = 0 point
x_end <- xlims[2]-xoffset+(0.5*x_intervals) # <-- calculate the true x=24 point
# Create the X-ticks
new_x_axis <- seq(x_start,x_end,x_intervals)
# Plot the line, add the lext to y-axis, plot y-axis
lines(x=head(new_x_axis, -1), y=new_table_input$Mean.x, type="b",pch=19,lwd=2, ylim=c(ymin,ymax), col="black") # <-- plot the line of data type 2
mtext(expression(-log[10](italic(cfu))), side=4, line=2.5, col="black")
axis(side=4, col="black", col.axis="black")
par(new=T) # <-- plot a transparant new window
ymax <- ceiling(max(new_table_input$Mean, na.rm=TRUE))# Y-max value
ymin <- floor(min(new_table_input$Mean, na.rm=TRUE))# Y-min value
plot(m, ylim=c(ymin,ymax), axes=F, type="n", xlab="", ylab="") # <- plot new empty window, with the new y-axis values
# Calculate where X-axis starts, ends, and where the individual points are
xlims <- par('usr')[1:2] # <-- get Where the window of the plot starts and ends
xoffset <- par('plt')[4] # <-- get the offeset between the window and the actual start and end of the x-axis
x_intervals <- ((xlims[2]-xoffset)-(xlims[1]+xoffset))/length(new_table_input$Mean.x) # <-- calculate the step size
x_start <- xlims[1]+xoffset+(0.5*x_intervals) # <-- calculate the true x = 0 point
x_end <- xlims[2]-xoffset+(0.5*x_intervals) # <-- calculate the true x=24 point
# Create the X-ticks
new_x_axis <- seq(x_start,x_end,x_intervals)
# Plot the line, add the lext to y-axis, plot y-axis
lines(x=head(new_x_axis, -1), y=new_table_input$Mean, type="b",pch=21,lwd=2, ylim=c(ymin,ymax), col="black") # <-- plot the line of data type 2
mtext(expression("Normalized cell OD"),side=4,line=6, col="black")
axis(side=4, line=4, col="black", col.axis="black")
legend("topleft",legend=c("Absorption", expression(-log[10](italic(cfu))), "Cell OD"), lty=c(0,1,1), pch=c(15 ,19, 21), col=c("black", "black", "black"), bty = "n")
