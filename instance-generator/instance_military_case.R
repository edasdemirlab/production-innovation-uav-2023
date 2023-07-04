# #Load Packages
# library(Matrix)
library(dplyr)
library(reshape2)
library(jpeg)
library(plotrix)

#Set Global Parameters
options(max.print = 99999)
set.seed(594)

# read data
target_data <- as.data.frame(read.table("military_case_input/target_attributes.txt",header = TRUE))
target_radius <- 80
target_data["target_area"] <- pi * target_radius^2
coordinate_data <- as.data.frame(read.table("military_case_input/node_coordinates.txt",header = TRUE))


# problem Parameters
n_targets <- 12  # number of targets excluding origin and destination
n_nodes <- n_targets + 1  # number of nodes including origin and destination
discount_ratio <- 30  # maximum discount in terms of %
mission_area <- 400*400
eps <- 0.1
flight_v <- 10*60  # flight speed (km per hour)
sensor_range <- 20

# search time reduction parameter
alpha_time_reduction <- 0.8

#search and collection duration
s_t <- 50/60
c_t <- 10/60
search_duration <- s_t + c_t

#effectiveness of the search and collection sensors
e_s <- (1-exp(-(flight_v*sensor_range)/((target_data$target_area)))^s_t) #effectiveness of search sensor - by Xia's paper--28km is based on Global Hawk
e_c <- 0.5 #effectiveness of collection sensor - by Moskal's previous papers

#restrictions
n_revisits <- 3  # number of allowed revisits

# problem Sets
target_set <- 1:n_targets
node_set <- 0:n_nodes
from_set <- 0:n_targets #target set including origin excluding destination #1 is origin
to_set <- 1:n_nodes
time_step_set <- (1:((n_targets*n_revisits)+2)) #50 ((n_targets*n_revisits)+2)
allowed_revisit_set <- (1:n_revisits) # set of allowed revisits


# problem plot
plot(c(), xlim=c(-35,920), ylim=c(-35,630), axes=T, xlab="", ylab="",xaxs="i", yaxs="i",asp=1 ) # see ?plot.function
jp <- readJPEG('military_case_input/map.jpg')
rasterImage(jp, xleft=-80, xright=947, ybottom=-35, ytop=630, )
box()
points(x = filter(coordinate_data, node_id==0)$x+32,y=filter(coordinate_data, node_id==0)$y+1,col = "black", pch=17,cex=1.5)
for (i in target_data$node_id) {
  
  target_now <- filter(target_data, node_id==i)
  coord_now <- filter(coordinate_data, node_id==i)

  # rect(coord_now$x - target_now$side_length,coord_now$y - target_now$side_length,
  #      coord_now$x + target_now$side_length,coord_now$y + target_now$side_length, lty=3,border = "black", lwd=2, col = rgb(0, 191, 255, max = 255, alpha = 50, names = "blue50"))

  points(x = coord_now$x-5, y=coord_now$y, col = "black", pch = 215,cex = 1, lwd = 4) 
  draw.circle(coord_now$x,coord_now$y,target_radius, lty=2,border = "black", lwd=2 ) #fena degil
  
  
  text(x = coord_now$x + 12,y = coord_now$y,labels = i,col = "black",cex = 0.8)
  
}

legend("top", inset = c(-0.003,-0.11), legend = c("Base", "Target's Center", "Target's Search Region"), xpd = TRUE, 
       horiz = TRUE,  col=c("black","black","black"),pch = c(17, 215, 1), bty = "n", pt.cex=1.5,text.width=c(200,370,90))



#generate all movement types
temp_list <- c()
pair_matrix <- c()
for (d in 0:(n_nodes-1)) {
  temp_list <- cbind(rep(d,(n_nodes)), 0:n_targets)
  pair_matrix <- rbind(pair_matrix, temp_list)
}
pair_matrix <- pair_matrix[-1,]

#---------------------------------------------------------------------------------------------------------------
# travel duration --> d_{ij}

#Calculate Euclidean Distances
euc_distances <- apply(pair_matrix, 1, function(x){
  pair_from_id = x[1]
  pair_to_id = x[2]
  pair_from_coord <- filter(coordinate_data, node_id == pair_from_id)
  pair_to_coord <- filter(coordinate_data, node_id == pair_to_id)
  sqrt((pair_from_coord$x - pair_to_coord$x)^2 + (pair_from_coord$y - pair_to_coord$y)^2)
})

pair_matrix <- cbind(pair_matrix, round(euc_distances/flight_v,4))
trajectory_df <- as.data.frame(pair_matrix)
colnames(trajectory_df) <- c('from_node', "to_node", "travel_duration")



# Update distances with discounted euclidean distances. Discount rates linearly increase from 0 to discount_ratio (%30 in the paper)
discount_rates <- unlist(lapply(pair_matrix[,3],function (x) (discount_ratio/(max(pair_matrix[,3]) - min(pair_matrix[,3])))*(x - min(pair_matrix[,3]))))
discount_amounts <- (pair_matrix[,3] * discount_rates)/100
pair_matrix[,3] <- (pair_matrix[,3] - discount_amounts)
pair_matrix[,3] <- round(pair_matrix[,3]/flight_v,4)

x_disc_rate <- cbind(pair_matrix[,1:2], discount_rates)
colnames(x_disc_rate) <- NULL

# convert distance to flight duration
pair_matrix[,3] <- round(pair_matrix[,3]/flight_v,4)


#---------------------------------------------------------------------------------------------------------------
# information search and collection time s_{j,m}
search_duration_list <- sapply(1:n_revisits, function(x) search_duration*(alpha_time_reduction)^(x-1))

#Construct Y matrix:
target_df <- data.frame(target_id = target_set)
for (m in 1:n_revisits){
  target_df[paste0(m)] = search_duration_list[m]
}

target_df <- melt(target_df, id.var = c('target_id') , variable.name = 'visit_id')
target_df <- target_df %>% rename("search_duration" = "value")
target_df['target_id'] = as.integer(target_df$target_id)
target_df['visit_id'] = as.integer(target_df$visit_id)

# ---------------------------------------------------------------------------------------------------
# expected Information Collection From Targets I_{jk}-> information collected from target j at kth visit
# probability that a target has information \mu_{pI_j}
prob_inf_exists <- target_data$info_prob
prob_inf_exists_permanent <- prob_inf_exists


target_info_df <- data.frame(matrix(ncol = 3, nrow = 0))
for (m in 1:n_revisits){
  target_info_df <- rbind(target_info_df,cbind(target_set, m, round((prob_inf_exists * e_s * ((1 - e_c)^(m-1) - (1-e_c)^m)),4) ))
}
colnames(target_info_df) <- c("target_id","visit_id", "expected_information")
target_df <- inner_join(target_df,target_info_df,by = c('target_id' = 'target_id', 'visit_id' = 'visit_id'))



#---------------------------------------------------------------------------------------------------------------
#Risk of being detected
# lambda_j <- round(c(runif(n_targets, 0, 0.05)), 4)
detect_rates <- round(target_data$detection_rates, 4) #number of detection per 1 hour flight
lamda_j <- c(0,as.vector(t(detect_rates)))
lamda_base <- 0.01

# radar exposure when visiting targets
target_df['number_of_detections'] <- target_df$search_duration * rep((tail(lamda_j, -1) + lamda_base), n_revisits)



#radar exposure at the trajectories
# Constructing delta
delta<-function(a,b,c){
  b^2-4*a*c
}

intsects <- function(a,b,c){
  if(delta(a,b,c) > 0){ # first case D>0
    x_1 = (-b+sqrt(delta(a,b,c)))/(2*a)
    x_2 = (-b-sqrt(delta(a,b,c)))/(2*a)
    intsects = c(x_1,x_2)
  }
  else if(delta(a,b,c) == 0){ # second case D=0
    x = -b/(2*a)
  }
  else {NA} # third case D<0
}

# x=trajectory_df[79,]

lambda_traj <- apply(trajectory_df , 1 ,function(x){
  x <- as.numeric(x)
  if( x[3] == 0) {traj_risk <- 0} else{
    # equation of the line
    pair_from_node <- x[1]
    pair_to_node <- x[2]
    
    sp <- as.numeric(filter(coordinate_data, node_id == pair_from_node))
    fp <- as.numeric(filter(coordinate_data, node_id == pair_to_node))

    slp <- (fp[3] - sp[3])/(fp[2] - sp[2]) # slope
    icept <- sp[3] - slp*sp[2] # intercept
    
    #look at each target for whether the line intersects the target or no
    target_candidates <- setdiff(target_set,unlist(x[1:2]))
    target_candidates_info <- as.matrix(filter(coordinate_data, node_id %in% target_candidates))
    colnames(target_candidates_info) <- NULL
    
    allicepts <- apply(target_candidates_info,1,function(y){
      eqA <- (1+slp^2)
      eqB <- ((-2*y[2])+(2*slp*icept)-(2*y[3]*slp))
      eqC <- (y[2]^2+icept^2+y[3]^2-target_radius^2-(2*y[3]*icept))

      roots_x <- intsects(eqA,eqB,eqC)
      roots_y <- sqrt(target_radius^2-(roots_x-y[2])^2)+y[3]
      if(length(roots_x)>1){
        if(roots_x[1]>=min(sp[2],fp[2]) & roots_x[1]<=max(sp[2],fp[2]) ){
          cbind(roots_x,roots_y,y[1])
        }else{}
      }else{}
    })
    if(length(allicepts) >= 1){
      
      allicepts <- do.call("rbind",allicepts) 
      allicepts <- rbind(c(sp[2:3],sp[1]),allicepts,c(fp[2:3],fp[1]))
      allicepts <- allicepts[order(allicepts[,1]),]
      allicepts2 <- cbind(head(allicepts,-1),tail(allicepts,-1))
      allicepts2 <- cbind(allicepts2,apply(allicepts2,1,function(z){
      sqrt((z[1] - z[4])^2 + (z[2] - z[5])^2)
      }))
      allicepts2 <- allicepts2[,c(3,6,7)]
      allicepts3 <- tail(head(allicepts2,-1),-1)
      w_discrate <- x_disc_rate[which(x_disc_rate[,1] == x[1] & x_disc_rate[,2] == x[2]),3] #discount rate of x
      from_detection<-(target_radius*lamda_j[allicepts2[1,1]+1] + ((allicepts2[1,3]-target_radius)*lamda_base))*((100-w_discrate)/100)
      to_detection<-(target_radius*lamda_j[allicepts2[nrow(allicepts2),2]+1] + ((allicepts2[nrow(allicepts2),3]-target_radius)*lamda_base))*((100-w_discrate)/100)
      
      #w<-allicepts3[1,]
      inter_decetions<-  apply(allicepts3,1,function(w){
        
        if(w[1]==w[2]){(lamda_j[w[1]+1]*w[3])*((100-w_discrate)/100)}else{
          ((lamda_base*w[3])*((100-w_discrate)/100))
        }
        
      })
      
      traj_risk <- sum(c(from_detection,inter_decetions,to_detection))/flight_v
      
    }else{
      
      w_discrate <- x_disc_rate[which(x_disc_rate[,1]==x[1] & x_disc_rate[,2]==x[2]),3] #discount rate of x
      dist_temp <- x[3]/((100-w_discrate)/100)*flight_v #not discounted travel time
      detection_temp <- (target_radius*lamda_j[x[1]+1] + ((dist_temp-2*target_radius)*lamda_base) + target_radius*lamda_j[x[2]+1] )*((100-w_discrate)/100)
      traj_risk <- detection_temp/flight_v
    }
  }
  return(traj_risk)
})
trajectory_df['number_of_detections'] <- round(lambda_traj, 4)
target_df