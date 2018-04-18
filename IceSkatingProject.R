#############################################################################################
#Figure Skating Analysis Using Monte Carlo Simulation
#Author: James Lee(jlee73@ncsu.edu)
#Abstract: The goal of this project is to understand FS program scoring and 
#          simulate the distribution of score before and after IJS rule change.
#############################################################################################

####load csv file
#change directory if needed
Player <- read.csv(file="D:/ISU dataset/Yuzuru Hanyu.csv", header=FALSE, sep=",")

####The data is scrapped from ISU website, which needs data-cleaning

#Type,exclude some irrelavent names
Type<-unique(Player[,1])
rm_type<-c("Elements","Program components","Total","Skating Skills","Transitions","Performance/Execution",
           "Composition","Interpretation","Deduction","Program total","Transition/Linking Footwork",
           "Choreography/Composition","Transition/Linking Footwork/Movement","Choreography")
rm_index<-which(Type %in% rm_type)

###compute how many times each type of jumps have perform and plot a barplot
##table type
Type<-Type[-rm_index]
par(mar = c(7, 4, 2, 2) + 0.2)
Type_count<-table(Player[,1],exclude=rm_type)

##Drawing barplot
#decide end point, the frame of the plot
end_point = 0.5 + nrow(Type_count) + nrow(Type_count)-1
#barplot-delete axisnames and put on rotated ones using text() syntax
barplot(Type_count,main="Number of Attempts",col="grey50", ylim=c(0,5+max(Type_count))
        ,ylab="Attempts",xlab="",space=1,axisnames = FALSE)
text(seq(1.5,end_point,by=2), par("usr")[3]-0.25, 
     srt = 60, adj= 1, xpd = TRUE,
     labels = paste(rownames(Type_count)), cex=0.65)

#define all single jump
four_toe<-c("4T","4T<","4T<<","4T+REP","4T<+REP")
four_shal<-c("4S","4S<","4S<<","4S+REP")
four_lutz<-c("4Lz","4Lz<","4Lz<<","4Lze","4Lz+REP")
four_loop<-c("4Lo","4Lo<","4Lo<<","4Lz+REP")
four_flip<-c("4F","4F<","4F<<","4Fe")
three_toe<-c("3T","3T<","3T<<")
three_shal<-c("3S","3S<","3S<<")
three_lutz<-c("3Lz","3Lz<","3Lz<<","3Lze")
three_loop<-c("3Lo","3Lo<","3Lo<<")
three_flip<-c("3F","3F<","3F<<","3Fe")
three_axel<-c("3A","3A<","3A<<")


QuadT <- Player[Player$V1 %in% four_toe,1:3]
QuadS <- Player[Player$V1 %in% four_shal,1:3]
QuadLz <- Player[Player$V1 %in% four_lutz,1:3]
QuadLo <- Player[Player$V1 %in% four_loop,1:3]
QuadF <- Player[Player$V1 %in% four_flip,1:3]
TripT <- Player[Player$V1 %in% three_toe,1:3]
TripS <- Player[Player$V1 %in% three_shal,1:3]
TripLz <- Player[Player$V1 %in% three_lutz,1:3]
TripLo <- Player[Player$V1 %in% three_loop,1:3]
TripF <- Player[Player$V1 %in% three_flip,1:3]
TripA <- Player[Player$V1 %in% three_axel,1:3]

##creating table for specific barplot on single jumps
SingleJump <- matrix(c(0,1,0,7,32,45,
                       18,34,45,33,6,2),ncol=1,byrow=TRUE)
colnames(SingleJump) <- c("Attempts")
rownames(SingleJump) <- c("QuadAxel","QuadLutz","QuadFlip","QuadLoop","QuadSalchow","QuadToe"
                          ,"TripAxel","TripLutz","TripFlip","TripLoop","TripSalchow","TripToe")
SingleJump <- as.table(SingleJump)
#create barplot
barplot(t(SingleJump),main="Single Jump",col="grey50",
        ylab="Attempts",xlab="")

##creating table for specific barplot on combination jumps

CombinationJump <- table(Player[,1],exclude=rm_type)

#create barplot
barplot(t(CombinationJump),main="Single Jump",col="grey50",
        ylab="Attempts",xlab="")

##creating table for specific barplot on Spins

Spins <- table()

#create barplot
barplot(Spins,main="Spins",col="grey50",
        ylab="Attempts",xlab="")
