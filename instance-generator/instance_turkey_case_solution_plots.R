library(plotrix)
library(jpeg)
library(dplyr)
library("readxl")
library(diagram)

myArrow <- function(x0, y0, x1, y1, cut = 1, ...){
  x.new <- (1 - cut) * x0 + cut * x1
  y.new <- (1 - cut) * y0 + cut * y1
  segments(x0, y0, x1, y1, ...)
  arrows(x0, y0, x.new, y.new, ...)
}

revisit_set <- c(1,2,3)

target_data <- as.data.frame(read.table("turkey_case_input/turkey_data.txt",header = TRUE))


x_tijk <- as.data.frame(read_excel("turkey_case_input/single_objective_results_10_nodes_2023_06_29_00_41.xlsx", sheet = "x_tijk_results"))
x_tijk <- x_tijk[,-1]

y_jmk <- as.data.frame(read_excel("turkey_case_input/single_objective_results_10_nodes_2023_06_29_00_41.xlsx", sheet = "y_jmk_results"))
y_jmk <- y_jmk[,-1]


# problem plot
plot(c(), xlim=c(0,2000), ylim=c(0,875), axes=T, xlab="", ylab="") # see ?plot.function
jp <- readJPEG('turkey_case_input/map_blank2.jpg')
rasterImage(jp, xleft=-115, xright=2100, ybottom=-35, ytop=880)
box()
points(x = filter(target_data, target_id == 0)$x,y=filter(target_data, target_id == 0)$y,col = "black", pch=17,cex = 2.5)
text(x = filter(target_data, target_id == 0)$x+50,y=filter(target_data, target_id == 0)$y+20,labels = 0,col = "black",cex = 1.5)

for (i in tail(target_data$target_id,-1)) {
  
  coord_now <- filter(target_data, target_id==i)
  points(x = coord_now$x-5, y=coord_now$y, col = "black", pch = 215,cex = 1, lwd = 4) 
  draw.circle(coord_now$x,coord_now$y,coord_now$border_length, lty=2,border = "black", col = adjustcolor("grey40", alpha.f = 0.2), lwd=2 ) #fena degil
  
  
  text(x = coord_now$x - 85,y = coord_now$y-55,labels = i,col = "black",cex = 1.5)
  
}

legend("top", inset = c(-0.003,-0.11), legend = c("base", "target center", "target search area"), xpd = TRUE, 
       horiz = TRUE,  col=c("black","black","black"),pch = c(17, 215, 1), bty = "n", pt.cex=1.5,text.width=c(280,580,90))


vehicle_set <- 1:4
col_set <- c("forestgreen", "red", "blue", "orange")
lty_set <- c("solid", "twodash", "dotted", "longdash" )
v=2
for (v in vehicle_set){
  vehicle_results_df <- x_tijk %>% filter(vehicle_id == v)
  apply(vehicle_results_df, 1, function(x){
    frm <- x[["from_node_id"]]
    to <- x[["to_node_id"]]
    if( frm != to){
      x0 <- filter(target_data, target_id == frm)$x
      y0 <- filter(target_data, target_id == frm)$y
      x1 <- filter(target_data, target_id == to)$x
      y1 <- filter(target_data, target_id == to)$y
      myArrow(x0, y0, x1, y1, lty = lty_set[v], length = 0.20, angle = 30, code=2, col=col_set[v], lwd=3,cut=0.6) 
    }
  })
  
  
  for (r in revisit_set){
    revisit_results_df <- y_jmk %>% filter(revisit_number  == r, vehicle_id == v)
    if(nrow(revisit_results_df) >=1){
    pos1_x <- filter(target_data, target_id %in% revisit_results_df$target_id)$x
    pos1_y <- filter(target_data, target_id %in% revisit_results_df$target_id)$y
    pos1 <- cbind(pos1_x, pos1_y)
    
    if(r == 1){
      ajduster <- 70
    }else if(r==2){
      ajduster <- 50
    } else{
      ajduster <- 30
    }

    pos1 <- cbind(pos1[,1]-ajduster,pos1[,2])
    
    for (i in 1:nrow(pos1)){
      selfarrow(pos = pos1[i,], path = "R", arr.pos = 1,
                curve = c(ajduster, ajduster), lcol = col_set[v],lty= lty_set[v], lwd=2)
    
    }}}
  
  
  
}






