#############################################################################################
#Figure Skating Analysis Using Monte Carlo Simulation
#Author: James Lee(jlee73@ncsu.edu)
#Abstract: The goal of this project is to understand FS program scoring and 
#          simulate the distribution of score before and after IJS rule change.
#############################################################################################

####load csv file
#change directory if needed
Player <- read.csv(file="D:/ISU dataset/Monte-Carlo-On-Ice/Yuzuru Hanyu.csv", header=FALSE, sep=",")

####The data is scrapped from ISU website, which needs data-cleaning

#Type,exclude some irrelavent names
Type<-unique(Player[,1])
rm_type<-c("Elements","Program components","Total","Skating Skills","Transitions","Performance/Execution",
           "Composition","Interpretation","Deduction","Program total","Transition/Linking Footwork",
           "Choreography/Composition","Transition/Linking Footwork/Movement","Choreography")
rm_index<-which(Type %in% rm_type)

##compute how many times each type of jumps have perform and plot a barplot
#table type
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


#define all jump

S1A_M<-Player[Player$V1 %in% c("1A"),1:3]
C1A1Lo3S_M<-Player[Player$V1 %in% c("1A+1Lo+3S"),1:3]
S1A1T_M<-Player[Player$V1 %in% c("1A+1T"),1:3]
S1Lo_M<-Player[Player$V1 %in% c("1Lo"),1:3]
S1Lz_M<-Player[Player$V1 %in% c("1Lz"),1:3]
C1Lz2T_M<-Player[Player$V1 %in% c("1Lz+2T"),1:3]
S1S_M<-Player[Player$V1 %in% c("1S"),1:3]
S2Lz_M<-Player[Player$V1 %in% c("2Lz"),1:3]
S2S_M<-Player[Player$V1 %in% c("2S"),1:3]
C2S1Lo_M<-Player[Player$V1 %in% c("2S+1Lo"),1:3]
C2S3T_M<-Player[Player$V1 %in% c("2S+3T"),1:3]
S2T_M<-Player[Player$V1 %in% c("2T"),1:3]
C2T1Lo2S_M<-Player[Player$V1 %in% c("2T+1Lo+2S"),1:3]
S3A_M<-Player[Player$V1 %in% c("3A","3A<"),1:3]
C3A1Lo2S_M<-Player[Player$V1 %in% c("3A+1Lo+2S","3A+1Lo<<+2S"),1:3]
C3A1Lo3S_M<-Player[Player$V1 %in% c("3A+1Lo+3S","3A+1Lo<+3S"),1:3]
C3A1T_M<-Player[Player$V1 %in% c("3A+1T"),1:3]
C3A1T1Lo_M<-Player[Player$V1 %in% c("3A+1T+1Lo"),1:3]
C3A2T_M<-Player[Player$V1 %in% c("3A+2T"),1:3]
C3A2T2T_M<-Player[Player$V1 %in% c("3A+2T+2T"),1:3]
C3A3T_M<-Player[Player$V1 %in% c("3A+3T","3A+3T<"),1:3]
C3A3T2T_M<-Player[Player$V1 %in% c("3A+3T+2T"),1:3]
S3F_M<-Player[Player$V1 %in% c("3F","3Fe"),1:3]
S3Lo_M<-Player[Player$V1 %in% c("3Lo","3Lo<"),1:3]
C3Lo1Lo3S_M<-Player[Player$V1 %in% c("3Lo+1Lo+3S"),1:3]
C3Lo1T_M<-Player[Player$V1 %in% c("3Lo+1T"),1:3]
C3Lo2T_M<-Player[Player$V1 %in% c("3Lo+2T"),1:3]
S3Lz_M<-Player[Player$V1 %in% c("3Lz","3Lz<"),1:3]
C3Lz1Lo3S_M<-Player[Player$V1 %in% c("3Lz+1Lo+3S","3Lz+1Lo+3S<"),1:3]
C3Lz1T_M<-Player[Player$V1 %in% c("3Lz+1T"),1:3]
C3Lz2T_M<-Player[Player$V1 %in% c("3Lz+2T"),1:3]
C3Lz2T2T_M<-Player[Player$V1 %in% c("3Lz+2T+2T"),1:3]
S3S_M<-Player[Player$V1 %in% c("3S"),1:3]
C3S2T_M<-Player[Player$V1 %in% c("3S+2T"),1:3]
S3T_M<-Player[Player$V1 %in% c("3T"),1:3]
S4Lo_M<-Player[Player$V1 %in% c("4Lo","4Lo<","4Lo<<","4Lo+REP"),1:3]
S4Lz_M<-Player[Player$V1 %in% c("4Lz","4Lz<","4Lz<<","4Lze","4Lz+REP"),1:3]
S4S_M<-Player[Player$V1 %in% c("4S","4S<","4S<<","4S+REP"),1:3]
C4S3T_M<-Player[Player$V1 %in% c("4S+3T"),1:3]
S4T_M<-Player[Player$V1 %in% c("4T","4T<","4T<<","4T+REP","4T<+REP"),1:3]
C4T1Lo3S_M<-Player[Player$V1 %in% c("4T+1Lo+3S"),1:3]
C4T2T_M<-Player[Player$V1 %in% c("4T+2T"),1:3]
C4T3T_M<-Player[Player$V1 %in% c("4T+3T"),1:3]
CCoSp1_M<-Player[Player$V1 %in% c("CCoSp1"),1:3]
CCoSp2_M<-Player[Player$V1 %in% c("CCoSp2"),1:3]
CCoSp3_M<-Player[Player$V1 %in% c("CCoSp3"),1:3]
CCoSp3p3_M<-Player[Player$V1 %in% c("CCoSp3p3"),1:3]
CCoSp3p4_M<-Player[Player$V1 %in% c("CCoSp3p4"),1:3]
CCoSp4_M<-Player[Player$V1 %in% c("CCoSp4"),1:3]
ChSq_M<-Player[Player$V1 %in% c("ChSq"),1:3]
ChSq1_M<-Player[Player$V1 %in% c("ChSq1"),1:3]
ChSt1_M<-Player[Player$V1 %in% c("ChSt1"),1:3]
CiSt1_M<-Player[Player$V1 %in% c("CiSt1"),1:3]
CiSt3_M<-Player[Player$V1 %in% c("CiSt3"),1:3]
CiSt4_M<-Player[Player$V1 %in% c("CiSt4"),1:3]
CSSp4_M<-Player[Player$V1 %in% c("CSSp4"),1:3]
FCCoSp2_M<-Player[Player$V1 %in% c("FCCoSp2"),1:3]
FCCoSp2p2_M<-Player[Player$V1 %in% c("FCCoSp2p2"),1:3]
FCCoSp3_M<-Player[Player$V1 %in% c("FCCoSp3"),1:3]
FCCoSp3p3_M<-Player[Player$V1 %in% c("FCCoSp3p3"),1:3]
FCCoSp3p4_M<-Player[Player$V1 %in% c("FCCoSp3p4"),1:3]
FCCoSp4_M<-Player[Player$V1 %in% c("FCCoSp4"),1:3]
FCSSp3_M<-Player[Player$V1 %in% c("FCSSp3"),1:3]
FCSSp4_M<-Player[Player$V1 %in% c("FCSSp4"),1:3]
SlSt2_M<-Player[Player$V1 %in% c("SlSt2"),1:3]
SlSt3_M<-Player[Player$V1 %in% c("SlSt3"),1:3]
StSq2_M<-Player[Player$V1 %in% c("StSq2"),1:3]
StSq3_M<-Player[Player$V1 %in% c("StSq3"),1:3]
StSq4_M<-Player[Player$V1 %in% c("StSq4"),1:3]

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

# ##creating table for specific barplot on combination jumps
# 
# CombinationJump <- table(Player[,1],exclude=rm_type)
# 
# #create barplot
# barplot(t(CombinationJump),main="Single Jump",col="grey50",
#         ylab="Attempts",xlab="")
# 
# ##creating table for specific barplot on Spins
# 
# Spins <- table()
# 
# #create barplot
# barplot(Spins,main="Spins",col="grey50",
#         ylab="Attempts",xlab="")

###data preprocessing for the simulation

jump_new<-Player[,1:3]
jump_new<-jump_new[!(jump_new$V1=="Elements"|jump_new$V1=="Program components"|jump_new$V1=="Total"|
                       jump_new$V1=="Skating Skills"|jump_new$V1=="Transitions"|jump_new$V1=="Performance/Execution"|
                       jump_new$V1=="Composition"|jump_new$V1=="Interpretation"|jump_new$V1=="Deduction"|
                       jump_new$V1=="Program total"|jump_new$V1=="Transition/Linking Footwork"|
                       jump_new$V1=="Choreography/Composition"|jump_new$V1=="Transition/Linking Footwork/Movement"|
                       jump_new$V1=="Choreography"),]
jump_cleaned<-jump_new[order(jump_new$V1),]

#See how GOE is diversed in every jump
GOE<-as.numeric(as.character(jump_cleaned$V3))

barplot(as.vector(GOE),main="GOE",col="grey50",
                 ylab="",xlab="")

################################Here is the end of the data proprocessing########################################

### Simulation

n<-1000
#the average difference in
AESavg_easy<-8.75
AESavg_hard<-8.25
pts_easy<-rep(0,n)
pts_hard<-rep(0,n)

#AES scoring:simulate 9 judge, average the middle two for five components
AES_easy<-function(){
  total_AES<-0
  for (i in 1:5){
    total_AES<-total_AES+AESavg_easy+0.25*mean(sort(sample(1:5,9,replace=T))[2:8])
  }
  return(2*total_AES)
}

AES_hard<-function(){
  total_AES<-0
  for (i in 1:5){
    total_AES<-total_AES+AESavg_hard+0.25*mean(sort(sample(1:5,9,replace=T))[2:8])
  }
  return(2*total_AES)
}
for (i in 1:n){
  pts_easy[i]<-sample(as.numeric(as.character(S4T_M$V2)),1)+sample(as.numeric(as.character(S4T_M$V3)),1)+
    sample(as.numeric(as.character(S2S_M$V2)),1)+sample(as.numeric(as.character(S2S_M$V3)),1)+
    sample(as.numeric(as.character(S3F_M$V2)),1)+sample(as.numeric(as.character(S3F_M$V3)),1)+
    sample(as.numeric(as.character(FCCoSp4_M$V2)),1)+sample(as.numeric(as.character(FCCoSp4_M$V3)),1)+
    sample(as.numeric(as.character(StSq3_M$V2)),1)+sample(as.numeric(as.character(StSq3_M$V3)),1)+
    sample(as.numeric(as.character(C3A3T_M$V2)),1)+sample(as.numeric(as.character(C3A3T_M$V3)),1)+
    sample(as.numeric(as.character(C3A2T_M$V2)),1)+sample(as.numeric(as.character(C3A2T_M$V3)),1)+
    sample(as.numeric(as.character(S3Lo_M$V2)),1)+sample(as.numeric(as.character(S4T_M$V3)),1)+
    sample(as.numeric(as.character(C3Lz2T2T_M$V2)),1)+sample(as.numeric(as.character(C3Lz2T2T_M$V3)),1)+
    sample(as.numeric(as.character(S3Lz_M$V2)),1)+sample(as.numeric(as.character(S3Lz_M$V3)),1)+
    sample(as.numeric(as.character(ChSq1_M$V2)),1)+sample(as.numeric(as.character(ChSq1_M$V3)),1)+
    sample(as.numeric(as.character(CCoSp4_M$V2)),1)+sample(as.numeric(as.character(CCoSp4_M$V3)),1)+
    sample(as.numeric(as.character(FCSSp4_M$V2)),1)+sample(as.numeric(as.character(FCSSp4_M$V3)),1)+AES_easy()
  
  pts_hard[i]<-sample(as.numeric(as.character(S4T_M$V2)),1)+sample(as.numeric(as.character(S4T_M$V3)),1)+
    sample(as.numeric(as.character(S4T_M$V2)),1)+sample(as.numeric(as.character(S4T_M$V3)),1)+
    sample(as.numeric(as.character(S3F_M$V2)),1)+sample(as.numeric(as.character(S3F_M$V3)),1)+
    sample(as.numeric(as.character(FCCoSp4_M$V2)),1)+sample(as.numeric(as.character(FCCoSp4_M$V3)),1)+
    sample(as.numeric(as.character(StSq3_M$V2)),1)+sample(as.numeric(as.character(StSq3_M$V3)),1)+
    sample(as.numeric(as.character(C4S3T_M$V2)),1)+sample(as.numeric(as.character(C4S3T_M$V3)),1)+
    sample(as.numeric(as.character(S4T_M$V2)),1)+sample(as.numeric(as.character(S4T_M$V3)),1)+
    sample(as.numeric(as.character(S3Lz_M$V2)),1)+sample(as.numeric(as.character(S3Lz_M$V3)),1)+
    sample(as.numeric(as.character(C3A1Lo3S_M$V2)),1)+sample(as.numeric(as.character(C3A1Lo3S_M$V3)),1)+
    sample(as.numeric(as.character(S3Lo_M$V2)),1)+sample(as.numeric(as.character(S3Lo_M$V3)),1)+
    sample(as.numeric(as.character(ChSq1_M$V2)),1)+sample(as.numeric(as.character(ChSq1_M$V3)),1)+
    sample(as.numeric(as.character(CCoSp4_M$V2)),1)+sample(as.numeric(as.character(CCoSp4_M$V3)),1)+
    sample(as.numeric(as.character(FCSSp4_M$V2)),1)+sample(as.numeric(as.character(FCSSp4_M$V3)),1)+AES_hard()
}
par(mfrow=c(2,1))
hist(pts_easy,xlim=range(165,215),ylim=range(0,180),main="Easy vs. Hard Before Rule Change")
hist(pts_hard,xlim=range(165,215),ylim=range(0,180),main="")
sum(as.numeric(pts_easy<pts_hard))

##change BV/ GOE based on ISU rule change
#deduct pts simply by subtractions 
#which may be different if the jump is downgraded or in the 2nd half
S4T_M["V4"]<-as.numeric(as.character(S4T_M$V2))-0.8
S4T_M["V5"]<-as.numeric(as.character(S4T_M$V3))*5/3

C3A3T_M["V4"]<-as.numeric(as.character(C3A3T_M$V2))-0.6
C3A3T_M["V5"]<-as.numeric(as.character(C3A3T_M$V3))*5/3

C3A2T_M["V4"]<-as.numeric(as.character(C3A2T_M$V2))-0.5
C3A2T_M["V5"]<-as.numeric(as.character(C3A2T_M$V3))*5/3

S3Lo_M["V4"]<-as.numeric(as.character(S3Lo_M$V2))-0.2
S3Lo_M["V5"]<-as.numeric(as.character(S3Lo_M$V3))*5/3

C3Lz2T2T_M["V4"]<-as.numeric(as.character(C3Lz2T2T_M$V2))-0.8
C3Lz2T2T_M["V5"]<-as.numeric(as.character(C3Lz2T2T_M$V3))*5/3

S3Lz_M["V4"]<-as.numeric(as.character(S3Lz_M$V2))-0.1
S3Lz_M["V5"]<-as.numeric(as.character(S3Lz_M$V3))*5/3

S4S_M["V4"]<-as.numeric(as.character(S4S_M$V2))-0.8
S4S_M["V5"]<-as.numeric(as.character(S4S_M$V3))*5/3

C4S3T_M["V4"]<-as.numeric(as.character(C4S3T_M$V2))-0.9
C4S3T_M["V5"]<-as.numeric(as.character(C4S3T_M$V3))*5/3

C3A1Lo3S_M["V4"]<-as.numeric(as.character(C3A1Lo3S_M$V2))-0.6
C3A1Lo3S_M["V5"]<-as.numeric(as.character(C3A1Lo3S_M$V3))*5/3

pts_easy_after<-rep(0,n)
pts_hard_after<-rep(0,n)
for (i in 1:n){
  pts_easy_after[i]<-sample(as.numeric(as.character(S4T_M$V4)),1)+sample(as.numeric(as.character(S4T_M$V5)),1)+
    sample(as.numeric(as.character(S2S_M$V2)),1)+sample(as.numeric(as.character(S2S_M$V3)),1)+
    sample(as.numeric(as.character(S3F_M$V2)),1)+sample(as.numeric(as.character(S3F_M$V3)),1)+
    sample(as.numeric(as.character(FCCoSp4_M$V2)),1)+sample(as.numeric(as.character(FCCoSp4_M$V3)),1)+
    sample(as.numeric(as.character(StSq3_M$V2)),1)+sample(as.numeric(as.character(StSq3_M$V3)),1)+
    sample(as.numeric(as.character(C3A3T_M$V4)),1)+sample(as.numeric(as.character(C3A3T_M$V5)),1)+
    sample(as.numeric(as.character(C3A2T_M$V4)),1)+sample(as.numeric(as.character(C3A2T_M$V5)),1)+
    sample(as.numeric(as.character(S3Lo_M$V4)),1)+sample(as.numeric(as.character(S4T_M$V5)),1)+
    sample(as.numeric(as.character(C3Lz2T2T_M$V4)),1)+sample(as.numeric(as.character(C3Lz2T2T_M$V5)),1)+
    sample(as.numeric(as.character(S3Lz_M$V4)),1)+sample(as.numeric(as.character(S3Lz_M$V5)),1)+
    sample(as.numeric(as.character(ChSq1_M$V2)),1)+sample(as.numeric(as.character(ChSq1_M$V3)),1)+
    sample(as.numeric(as.character(CCoSp4_M$V2)),1)+sample(as.numeric(as.character(CCoSp4_M$V3)),1)+
    sample(as.numeric(as.character(FCSSp4_M$V2)),1)+sample(as.numeric(as.character(FCSSp4_M$V3)),1)+AES_easy()
  
  pts_hard_after[i]<-sample(as.numeric(as.character(S4T_M$V4)),1)+sample(as.numeric(as.character(S4T_M$V5)),1)+
    sample(as.numeric(as.character(S4T_M$V4)),1)+sample(as.numeric(as.character(S4T_M$V5)),1)+
    sample(as.numeric(as.character(S3F_M$V2)),1)+sample(as.numeric(as.character(S3F_M$V3)),1)+
    sample(as.numeric(as.character(FCCoSp4_M$V2)),1)+sample(as.numeric(as.character(FCCoSp4_M$V3)),1)+
    sample(as.numeric(as.character(StSq3_M$V2)),1)+sample(as.numeric(as.character(StSq3_M$V3)),1)+
    sample(as.numeric(as.character(C4S3T_M$V4)),1)+sample(as.numeric(as.character(C4S3T_M$V5)),1)+
    sample(as.numeric(as.character(S4T_M$V4)),1)+sample(as.numeric(as.character(S4T_M$V5)),1)+
    sample(as.numeric(as.character(S3Lz_M$V4)),1)+sample(as.numeric(as.character(S3Lz_M$V5)),1)+
    sample(as.numeric(as.character(C3A1Lo3S_M$V4)),1)+sample(as.numeric(as.character(C3A1Lo3S_M$V5)),1)+
    sample(as.numeric(as.character(S3Lo_M$V4)),1)+sample(as.numeric(as.character(S3Lo_M$V5)),1)+
    sample(as.numeric(as.character(ChSq1_M$V2)),1)+sample(as.numeric(as.character(ChSq1_M$V3)),1)+
    sample(as.numeric(as.character(CCoSp4_M$V2)),1)+sample(as.numeric(as.character(CCoSp4_M$V3)),1)+
    sample(as.numeric(as.character(FCSSp4_M$V2)),1)+sample(as.numeric(as.character(FCSSp4_M$V3)),1)+AES_hard()
}
par(mfrow=c(2,1))
hist(pts_easy_after,breaks=15,xlim=range(150,230),ylim=range(0,180),main="Easy vs. Hard After Rule Change")
hist(pts_hard_after,breaks=15,xlim=range(150,230),ylim=range(0,180),main="")

