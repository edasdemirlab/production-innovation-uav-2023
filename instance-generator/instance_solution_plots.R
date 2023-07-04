# install.packages("plotrix")
library(plotrix)
library(jpeg)
setwd("C:/Users/Erdi Dasdemir/OneDrive/Belgeler/Erdi Research/(2020) Moskal UAV/Codes/CaseStudy/Civilian_Application")

nSubReg<-3
SensorRange<-28
s_t<- (50/60)/nSubReg
c_t<- 10/60
TargetRadius<-40
TargetEdge<-20
TargetArea<-pi * TargetRadius^2
s_e<- (1-exp(-(FlightV*SensorRange)/((TargetArea)/nSubReg))^s_t) #effectiveness of search sensor - by Xia's paper--28km is based on Global Hawk


fourt<-read.table("Reservoirs.txt",header = FALSE)
cap<-read.table("Capacities.txt",header = FALSE)

colnames(fourt)<-NULL
rownames(fourt)<-NULL


colnames(cap)<-NULL
rownames(cap)<-NULL

#      1        2       3      4       5       6      7       8        9      10      11      12 
xax=fourt[,1]
yax=fourt[,2]


notarget=58;
TargetRadius<-t(cap)
#1:4,532,771

plot(c(), xlim=c(-35,920), ylim=c(-35,630), axes=T, xlab="", ylab="",xaxs="i", yaxs="i",asp=1 ) # see ?plot.function

jp <- readJPEG('colorado_map.jpg')
rasterImage(jp, xleft=-80, xright=947, ybottom=-35, ytop=630, )
# rasterImage(jp, xleft=-80, xright=910, ybottom=-65, ytop=630)
box()

points(x=xax[1]+32,y=yax[1]+1,col = "black", pch=17,cex=1.5)
points(x=xax[nrow(fourt)]-28,y=yax[nrow(fourt)]-8,col = "black", pch=17,cex=1.5)

# points(x=xax[1]+32,y=yax[1]+1,col = "black", pch=17,cex=1.5)
# points(x=xax[nrow(fourt)]-28,y=yax[nrow(fourt)]-8,col = "black", pch=17,cex=1.5)
# 

i=2
for(i in 2:(nrow(fourt)-1)){
  
  rect(xax[i]-TargetRadius[i],yax[i]-TargetRadius[i],xax[i]+TargetRadius[i],yax[i]+TargetRadius[i], lty=3,border = "black", lwd=2, col = rgb(0, 191, 255, max = 255, alpha = 50, names = "blue50"))
  points(x=xax[i],y=yax[i],col = "black", pch=215,cex=1, lwd=4)    #iyi 
  
  rect(xax[i]-TargetRadius[i],yax[i]-TargetRadius[i],xax[i]+TargetRadius[i],yax[i]+TargetRadius[i], lty=3,border = "black", lwd=2 )
  
  
  # draw.circle(xax[i],yax[i],TargetRadius[i] , lty=2,border = "black", lwd=2 ) #fena degil
  text(x=xax[i]+13,y=yax[i],labels = i,col = "black",cex=0.8)
  
}


col2rgb("deepskyblue")


legend("top", inset = c(-0.003,-0.11), legend = c("Base", "Target's Center", "Target's Search Region"), xpd = TRUE, 
       horiz = TRUE,  col=c("black","black", "deepskyblue"),pch = c(17, 215, 15), bty = "n", pt.cex=1.5,text.width=c(170,320,40))





























#Set Working Directory 
setwd("C:/Users/Erdi Dasdemir/OneDrive/Belgeler/Erdi Research/(2020) Moskal UAV/Codes/CaseStudy/Civilian_Application_Revision/ThreeCases")

library(jpeg)
library(diagram)
#Load Packages
library(Matrix)
library(gurobi)
# install.packages("matchingR")
library(matchingR)
library(plotrix)

#Set Global Parameters
options(max.print = 99999)
set.seed(594)



myArrow <- function(x0, y0, x1, y1, cut = 1, ...){
  x.new <- (1 - cut) * x0 + cut * x1
  y.new <- (1 - cut) * y0 + cut * y1
  segments(x0, y0, x1, y1, ...)
  arrows(x0, y0, x.new, y.new, ...)
}


nSubReg<-3
SensorRange<-28
s_t<- (50/60)/nSubReg
c_t<- 10/60
TargetRadius<-40
TargetEdge<-20
TargetArea<-pi * TargetRadius^2
s_e<- (1-exp(-(FlightV*SensorRange)/((TargetArea)/nSubReg))^s_t) #effectiveness of search sensor - by Xia's paper--28km is based on Global Hawk


fourt<-read.table("Reservoirs.txt",header = FALSE)
cap<-read.table("Capacities.txt",header = FALSE)

colnames(fourt)<-NULL
rownames(fourt)<-NULL


colnames(cap)<-NULL
rownames(cap)<-NULL

#      1        2       3      4       5       6      7       8        9      10      11      12 
xax=fourt[,1]
yax=fourt[,2]
Coord<-cbind(xax,yax)

notarget=58;
TargetRadius<-t(cap)
TargetRadius2<-TargetRadius
TargetRadius<-c(0,TargetRadius,0)

#1:4,532,771

plot(c(), xlim=c(-35,920), ylim=c(-35,630), axes=T, xlab="", ylab="",xaxs="i", yaxs="i",asp=1 ) # see ?plot.function
box()
points(x=xax[1],y=yax[1],col = "black", pch=17,cex=1.5)
points(x=xax[nrow(fourt)],y=yax[nrow(fourt)],col = "black", pch=17,cex=1.5)
text(x=xax[1],y=yax[1]-20, labels = "h",col = "black", pch=17,cex=1)
text(x=xax[nrow(fourt)],y=yax[nrow(fourt)]-20,labels = "d",col = "black", pch=17,cex=1)
i=2
for(i in 2:(nrow(fourt)-1)){
  
  rect(xax[i]-TargetRadius[i],yax[i]-TargetRadius[i],xax[i]+TargetRadius[i],yax[i]+TargetRadius[i], lty=3,border = "black", lwd=2)
  points(x=xax[i],y=yax[i],col = "black", pch=215,cex=0.8, lwd=4)    #iyi 
  
  
  
  # draw.circle(xax[i],yax[i],TargetRadius[i] , lty=2,border = "black", lwd=2 ) #fena degil
  text(x=xax[i]+13,y=yax[i],labels = i,col = "black",cex=0.8)
  
}

#3-->4
FrmToMatrix<-matrix(c(1,18,18,18,18,18,18,11,11,11,11,11,11,23,23,29,29,29,29,3,3,3,3,3,3,29,29,27,27,26,26,26,26,27,27,8,8,8,8,15,15,15,15,10,10,2,2,2,2,13,13,13,13,2,2,10,10,10,10,5,5,5,5,12,12,24,24,24,24,12,12,12,12,5,5,4,4,4,4,4,4,9,9,9,9,9,9,22,22,22,22,7,7,7,7,25,25,20,20,25,25,7,7,16,16,16,16,14,14,28,28,28,28,28,28,14,14,14,14,16,16,22,22,23,23,23,23,6,6,20,20,6,6,6,6,17,17,17,17,17,17,20,20,25,25,26,26,27,27,8,8,15,15,13,13,24,24,19,19,19,19,19,19,21,21,21,21,21,21,30
),nrow=85, byrow = TRUE)
apply(FrmToMatrix,1,function(x){
  frm<-x[1]; to<-x[2]; 
  x0 <- Coord[frm,1] ; y0 <- Coord[frm,2]; x1 <- Coord[to,1] ; y1 <- Coord[to,2] ; x <- c(x0, x1) ; y <- c(y0, y1) 
  myArrow(x0, y0, x1, y1, lty=1, length = 0.15, angle = 30, code=2, col="limegreen", lwd=2,cut=0.6) 
})


#First Visit
# pos1 <- Coord[2:29,]
# pos1_r<-TargetRadius[2:29]-5
# for (i in 1:nrow(pos1)){
#   rect(pos1[i,1]-pos1_r[i],pos1[i,2]-pos1_r[i],pos1[i,1]+pos1_r[i],pos1[i,2]+pos1_r[i], lty=1,border = "limegreen", lwd=1)
#   points(pos1[i,1]-pos1_r[i],pos1[i,2]+10, pch=17,col="limegreen", cex=0.8)
# }



pos1 <- Coord[2:29,]
pos1 <- cbind(t(pos1[,1]-TargetRadius2),pos1[,2])
i=1
for (i in 1:nrow(pos1))
  selfarrow(pos =pos1[i,], path = "R", arr.pos = 0.3,
            curve = c(TargetRadius2[i], TargetRadius2[i]), lcol = "limegreen",lwd=1, arr.lwd = 0.1)

#Second Visit
# pos2 <- Coord[2:29,]
# pos2_r <- TargetRadius[2:29]/2
# 
# for (i in 1:nrow(pos2)){
#   rect(pos2[i,1]-pos2_r[i],pos2[i,2]-pos2_r[i],pos2[i,1]+pos2_r[i],pos2[i,2]+pos2_r[i], lty=1,border = "limegreen", lwd=1)
#   points(pos2[i,1]-pos2_r[i],pos2[i,2], pch=17,col="limegreen", cex=0.8)
# }



pos2 <- Coord[c(14,15),]
pos2 <- cbind(t(pos2[,1]-(TargetRadius2/1.5)),pos2[,2])
i=1
for (i in 1:nrow(pos2))
  selfarrow(pos =pos2[i,], path = "R", arr.pos = 0.5,
            curve = c(TargetRadius2[i]/1.5, TargetRadius2[i]/1.5), lcol = "limegreen",lwd=1,arr.lwd = 0.1)



#Third Visit
# pos3 <- Coord[2:29,]
# pos3_r <- TargetRadius[2:29]/4
# 
# for (i in 1:nrow(pos3)){
#   rect(pos3[i,1]-pos3_r[i],pos3[i,2]-pos3_r[i],pos3[i,1]+pos3_r[i],pos3[i,2]+pos3_r[i], lty=1,border = "limegreen", lwd=1)
#   points(pos3[i,1]-pos3_r[i],pos3[i,2]-10, pch=17,col="limegreen", cex=0.8)
# }


pos3 <- Coord[2:29,]
pos3 <- cbind(t(pos3[,1]-(TargetRadius2/3)),pos3[,2])
i=1
for (i in 1:nrow(pos3))
  selfarrow(pos =pos3[i,], path = "R", arr.pos = 0.9,
            curve = c(TargetRadius2[i]/3, TargetRadius2[i]/3), lcol = "limegreen",lwd=1,arr.lwd = 0.1)




#case 2
plot(c(), xlim=c(-35,920), ylim=c(-35,630), axes=T, xlab="", ylab="",xaxs="i", yaxs="i",asp=1 ) # see ?plot.function
box()
points(x=xax[1],y=yax[1],col = "black", pch=17,cex=1.5)
points(x=xax[nrow(fourt)],y=yax[nrow(fourt)],col = "black", pch=17,cex=1.5)
text(x=xax[1],y=yax[1]-20, labels = "h",col = "black", pch=17,cex=1)
text(x=xax[nrow(fourt)],y=yax[nrow(fourt)]-20,labels = "d",col = "black", pch=17,cex=1)
i=2
for(i in 2:(nrow(fourt)-1)){
  
  rect(xax[i]-TargetRadius[i],yax[i]-TargetRadius[i],xax[i]+TargetRadius[i],yax[i]+TargetRadius[i], lty=3,border = "black", lwd=2)
  points(x=xax[i],y=yax[i],col = "black", pch=215,cex=0.8, lwd=4)    #iyi 
  
  
  
  # draw.circle(xax[i],yax[i],TargetRadius[i] , lty=2,border = "black", lwd=2 ) #fena degil
  text(x=xax[i]+13,y=yax[i],labels = i,col = "black",cex=0.8)
  
}

#3-->4
FrmToMatrix<-matrix(c(
  1,18,18,11,11,23,23,23,23,23,23,29,29,29,29,3,3,15,15,2,2,2,2,13,13,13,13,24,24,19,19,19,19,21,21,24,24,24,24,24,24,12,12,12,12,12,12,10,10,10,10,10,10,5,5,5,5,5,5,4,4,9,9,9,9,9,9,4,4,4,4,4,4,8,8,8,8,27,27,27,27,26,26,26,26,6,6,17,17,20,20,20,20,25,25,25,25,25,25,7,7,16,16,14,14,28,28,28,28,14,14,14,14,14,14,30,
),nrow=58, byrow = TRUE)
apply(FrmToMatrix,1,function(x){
  frm<-x[1]; to<-x[2]; 
  x0 <- Coord[frm,1] ; y0 <- Coord[frm,2]; x1 <- Coord[to,1] ; y1 <- Coord[to,2] ; x <- c(x0, x1) ; y <- c(y0, y1) 
  myArrow(x0, y0, x1, y1, lty=1, length = 0.15, angle = 30, code=2, col="limegreen", lwd=2,cut=0.6) 
})




#First Visit
TargetRadius3<-TargetRadius[c(2,3,4,5,6,7,8,9,10,11,12,13,16,17,18,19,20,21,23,24,25,26,27,28,29)]
pos1 <- Coord[c(2,3,4,5,6,7,8,9,10,11,12,13,16,17,18,19,20,21,23,24,25,26,27,28,29),]
pos1 <- cbind(pos1[,1]-TargetRadius3,pos1[,2])
i=1
for (i in 1:nrow(pos1))
  selfarrow(pos =pos1[i,], path = "R", arr.pos = 0.3,
            curve = c(TargetRadius3[i], TargetRadius3[i]), lcol = "limegreen",lwd=1, arr.lwd = 0.01)




#Second Visit
TargetRadius4<-TargetRadius[c(2, 4, 5, 8, 9, 10, 13, 19, 20, 23, 24, 25, 26, 27, 28)]

pos2 <- Coord[c(2, 4, 5, 8, 9, 10, 13, 19, 20, 23, 24, 25, 26, 27, 28),]
pos2 <- cbind(pos2[,1]-TargetRadius4/1.5,pos2[,2])
i=1
for (i in 1:nrow(pos2))
  selfarrow(pos =pos2[i,], path = "R", arr.pos = 0.5,
            curve = c(TargetRadius4[i]/1.5, TargetRadius4[i]/1.5), lcol = "limegreen",lwd=1,arr.lwd = 0.01)



#Third Visit
pos3 <-  Coord[c(4,5,8,11,12,18,23,24,26,27),]
pos3 <- cbind(pos3[,1]-TargetRadius3/3,pos2[,2])
i=1
for (i in 1:nrow(pos3))
  selfarrow(pos =pos3[i,], path = "R", arr.pos = 0.9,
            curve = c(TargetRadius3[i]/3, TargetRadius3[i]/3), lcol = "limegreen",lwd=1,arr.lwd = 0.01)





#case 3
#---------------------------------------
plot(c(), xlim=c(-35,920), ylim=c(-35,630), axes=T, xlab="", ylab="",xaxs="i", yaxs="i",asp=1 ) # see ?plot.function
box()
points(x=xax[1],y=yax[1],col = "black", pch=17,cex=1.5)
points(x=xax[nrow(fourt)],y=yax[nrow(fourt)],col = "black", pch=17,cex=1.5)
text(x=xax[1],y=yax[1]-20, labels = "h",col = "black", pch=17,cex=1)
text(x=xax[nrow(fourt)],y=yax[nrow(fourt)]-20,labels = "d",col = "black", pch=17,cex=1)
i=2
for(i in 2:(nrow(fourt)-1)){
  
  rect(xax[i]-TargetRadius[i],yax[i]-TargetRadius[i],xax[i]+TargetRadius[i],yax[i]+TargetRadius[i], lty=3,border = "black", lwd=2)
  points(x=xax[i],y=yax[i],col = "black", pch=215,cex=0.8, lwd=4)    #iyi 
  
  
  
  # draw.circle(xax[i],yax[i],TargetRadius[i] , lty=2,border = "black", lwd=2 ) #fena degil
  text(x=xax[i]+13,y=yax[i],labels = i,col = "black",cex=0.8)
  
}

#xijk
FrmToMatrix1<-matrix(c(1,18,18,18,18,18,18,11,11,11,11,11,11,23,23,26,26,26,26,27,27,8,8,8,8,4,4,4,4,4,4,5,5,12,12,24,24,21,21,21,21,30),nrow=21, byrow = TRUE)

apply(FrmToMatrix1,1,function(x){
  frm<-x[1]; to<-x[2]; 
  x0 <- Coord[frm,1] ; y0 <- Coord[frm,2]; x1 <- Coord[to,1] ; y1 <- Coord[to,2] ; x <- c(x0, x1) ; y <- c(y0, y1) 
  myArrow(x0, y0, x1, y1, lty=1, length = 0.15, angle = 30, code=2, col="limegreen", lwd=2,cut=0.6) 
})




#First Visit
TargetRadius3<-TargetRadius[c(4,5,8, 11,12,18,21,23,24,26,27)]
pos1 <- Coord[c(4,5,8, 11,12,18,21,23,24,26,27),]
pos1 <- cbind(pos1[,1]-TargetRadius3,pos1[,2])
i=1
for (i in 1:nrow(pos1))
  selfarrow(pos =pos1[i,], path = "R", arr.pos = 0.35,
            curve = c(TargetRadius3[i], TargetRadius3[i]), lcol = "limegreen",lwd=1, arr.lwd = 0.01)




#Second Visit
TargetRadius4<-TargetRadius[c(8)]
pos2 <- Coord[c(8),]
pos2 <- cbind(pos2[1]-TargetRadius4/1.5,pos2[2])
selfarrow(pos =pos2, path = "R", arr.pos = 0.6,
          curve = c(TargetRadius4/1.5, TargetRadius4/1.5), lcol = "limegreen",lwd=1,arr.lwd = 0.01)













