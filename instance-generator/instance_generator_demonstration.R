# #Load Packages
# library(Matrix)
library(dplyr)
library(reshape2)

#Set Global Parameters
options(max.print = 99999)
set.seed(594)

#Problem Parameters
n_targets <- 16  # number of targets excluding origin and destination
n_nodes <- n_targets + 1  # number of nodes including origin and destination
discount_ratio <- 30  # maximum discount in terms of %
mission_area <- 400*400
eps <- 0.1
flight_v <- 10*60  # flight speed (km per hour)
sensor_range <- 28

# search time reduction parameter
alpha_time_reduction <- 0.8

#search and collection duration
s_t <- 50/60
c_t <- 10/60
search_duration <- s_t + c_t

#restrictions
mission_late_prob <- 0.20  # the allowed risk of not completing mission on time 
n_revisits <- 3  # number of allowed revisits


#effectiveness of the search and collection sensors
e_s <- (1 - exp(-(flight_v*sensor_range)/((mission_area/n_targets)))^s_t) #effectiveness of search sensor - by Xia's paper--28km is based on Global Hawk
e_c <- 0.5 #effectiveness of collection sensor - by Moskal's previous papers


# problem Sets
target_set <- 1:n_targets
node_set <- 0:n_nodes
from_set <- 0:n_targets #target set including origin excluding destination #1 is origin
to_set <- 1:n_nodes
time_step_set <- (1:((n_targets*n_revisits)+2)) #50 ((n_targets*n_revisits)+2)
allowed_revisit_set <- (1:n_revisits) # set of allowed revisits

# problem Layout
grid_size <- sqrt(mission_area)/sqrt(n_targets) #length of each grid
x_axis_size <- 400
y_axis_size <- 400
target_loc <- seq(grid_size/2, sqrt(mission_area)-grid_size/2,by = grid_size) 
target_loc_x <- rep(target_loc, each = sqrt(n_targets)) # X coordinates of targets
target_loc_y <- rep(target_loc, sqrt(n_targets)) # Y coordinates of targets
coord <- cbind(0:n_nodes,c(0,target_loc_x,sqrt(mission_area)),c(0,target_loc_y, sqrt(mission_area)))
coord <- head(coord, -1)

coord_grids_matrix <- cbind(target_set,rep(seq(0,300,100),each=4),rep(seq(100,400,100),each=4),rep(seq(0,300,100),4),rep(seq(100,400,100),4))

plot(x = 0, y = 0, xlim = c(0,sqrt(mission_area)), ylim = c(0,sqrt(mission_area)), pch = 17, cex = 2, asp = 1, xlab = "",ylab = "")  #xaxt="n", yaxt="n"
grid_loc <- seq(0,sqrt(mission_area), by = grid_size)
abline(h = grid_loc, v = grid_loc, col = "lightgray", lty = "dotted")
points(x = target_loc_x, y = target_loc_y, col = "black", pch = 215, cex=1)
text(coord[,2] - 10, coord[,3] - 10, labels = seq_along(0:n_targets)-1, cex = 0.8)
text(coord[1,2] - 10, coord[1,3] + 10, labels = "h", cex = 0.8)


#generate all movement types
temp_list <- c()
pair_matrix <- c()
for (d in 0:(n_nodes-1)) {
  temp_list <- cbind(rep(d,(n_nodes)), 0:n_targets)
  pair_matrix <- rbind(pair_matrix, temp_list)
}

#---------------------------------------------------------------------------------------------------------------
# travel duration --> d_{ij}

#Calculate Euclidean Distances
euc_distances <- apply(pair_matrix, 1, function(x){
  pair_from <- coord[coord[,1] == x[1],2:3]
  pair_to <- coord[coord[,1] == x[2],2:3]
  sqrt((pair_from[1] - pair_to[1])^2 + (pair_from[2] - pair_to[2])^2)
})

pair_matrix <- cbind(pair_matrix, euc_distances)



# Update distances with discounted euclidean distances. Discount rates linearly increase from 0 to discount_ratio (%30 in the paper)
discount_rates <- unlist(lapply(pair_matrix[,3],function (x) (discount_ratio/(max(pair_matrix[,3]) - min(pair_matrix[,3])))*(x - min(pair_matrix[,3]))))
discount_amounts <- (pair_matrix[,3] * discount_rates)/100
pair_matrix[,3] <- (pair_matrix[,3] - discount_amounts)

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
prob_inf_exists <- runif(n_targets, 0.5, 1)
prob_inf_exists_permanent <- prob_inf_exists

target_info_df <- data.frame(target_id = rep(target_set, each=n_revisits), visit_id = rep(c(1:n_revisits), n_targets))
target_info_df['expected_information'] <- unlist(lapply(prob_inf_exists, function(x){
  lapply(1:n_revisits, function(m){
    round(x * e_s * ((1 - e_c)^(m-1) - (1-e_c)^m),4)
  })
}))

target_df <- inner_join(target_df,target_info_df,by = c('target_id' = 'target_id', 'visit_id'='visit_id'))

#---------------------------------------------------------------------------------------------------------------
#Risk of being detected
lambda_j <- round(c(runif(n_targets, 0, 0.05)), 4) #number of detection per 1 hour flight
gridlines <- seq(0, x_axis_size, grid_size)


lambda_traj <- sapply(1:nrow(pair_matrix),function(s){
  
  i <- coord[coord[,1] == pair_matrix[s,1],2:3]
  j <- coord[coord[,1] == pair_matrix[s,2],2:3]

  
  if (pair_matrix[s,1] == pair_matrix[s,2])
    { 
      lambda_traj <- 0
    }
  else{
    #grid lines that are cut at x axis when moving right
    cuts_r_x <- gridlines[which(gridlines %in% seq(i[1], j[1]))]
    cuts_r_y <- unlist(lapply(cuts_r_x, function (xx) ((xx-i[1])*(j[2]-i[2]) + i[2]*(j[1]-i[1]))/(j[1]-i[1])))
    
    #grid lines that are cut at y axis when moving up
    cuts_u_y <- gridlines[which(gridlines %in% seq(i[2],j[2]))]
    cuts_u_x <- unlist(lapply(cuts_u_y, function (yy) ((j[1]-i[1])*(yy-i[2]) + i[1]*(j[2]-i[2]))/(j[2]-i[2])))
    
    cut_points <- rbind(cbind(cuts_r_x,cuts_r_y), cbind(cuts_u_x,cuts_u_y),i)
    cut_points <- cut_points[order(cut_points[,1]),]
    
    cut_points_M <- cbind(cut_points,rbind(cut_points[2:nrow(cut_points),],j))
    cut_points_dist <- apply(cut_points_M,1, function (e) sqrt((e[4]-e[2])^2 + (e[3]-e[1])^2))
    
    cut_points_dist <- cut_points_dist-(cut_points_dist*discount_rates[s])/100 #implement discounts
    cut_points_time <- cut_points_dist/flight_v
    
    
    cut_points_regions <- apply(cut_points_M,1, function (c){
      Cut_middle<-c((c*0.5)[1]+(c*0.5)[3],(c*0.5)[2]+(c*0.5)[4])
      Cut_region<-coord_grids_matrix[which(apply(coord_grids_matrix,1,function(x){
        Cut_middle[1]>=x[2] && Cut_middle[1]<=x[3] && Cut_middle[2]>=x[4] && Cut_middle[2]<=x[5]  
      })),1]
      Cut_region[1]
      
    })
    
    
    lambda_traj <- sum(lambda_j[cut_points_regions]*cut_points_time)
  }
  lambda_traj
  
})


# add threats to the trajectory pair data frame
trajectory_df <- as.data.frame(pair_matrix)
colnames(trajectory_df) <- c('from_node', "to_node", "travel_duration")
trajectory_df['number_of_detections'] <- lambda_traj

lambda_df <- data.frame(target_id = target_set, number_of_detections = lambda_j)
target_df <- inner_join(target_df,lambda_df,by = c('target_id' = 'target_id'))
target_df['number_of_detections'] <- target_df$search_duration * target_df$number_of_detections








#

missionTime<-missiontime_step_set[i]
beta_f<-Risallowed_revisit_set[i]
varTimeRestrict<-Vartime_step_set[i]

time_step_set<-time_step_setAll[[i]]

#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
#START MIP MODEL
#VARIABLE CONSTRUCTION

#xpij Binary Variables: Variables for Information obtained at location j (excluding destination)
#X_Matrix: #3: Travel Time, #4:Travel Time Variance, #5: Radar Detection Rate

xpij_Matrix<-rbind(
  cbind(1,X_M[1:(n_nodes-2),]),
  cbind(rep((2:tail(time_step_set,2)[1]),each=nrow(X_M[(n_nodes-1):(nrow(X_M)),])),
        
        do.call("rbind",replicate((length(time_step_set)-2),X_M[(n_nodes-1):(nrow(X_M)),],simplify=FALSE))),
  cbind(tail(time_step_set,1),X_M[which(X_M[,2]==n_nodes),])
)

rownames(xpij_Matrix) <- NULL

yjk_Matrix<-Y_M
rownames(yjk_Matrix) <- NULL

# Xord<-as.matrix(cbind(paste0(xpij_Matrix[,1],",",xpij_Matrix[,2],",",xpij_Matrix[,3])))
xpij_ord<-as.matrix(cbind(paste0(xpij_Matrix[,1],",",xpij_Matrix[,2],",",xpij_Matrix[,3] )))


xpij_names<-apply(xpij_Matrix[,1:3],1, function (x) paste0('x',",",x[1],",",x[2],",",x[3]))  
xpij_obj<-rep(0,length(xpij_names))

#yjk Binary Variables: Indicates  kth information collection from vertex j.
yjk_names<-apply(Y_M[,1:2],1, function (y) paste0('y',",",y[1],",",y[2]))
yjk_obj<-  Y_M[,5]*1000 #10000

# mu_Tf - Total Flight Duration of route f
# mu_Tf_max<-missionTime-(qnorm(1-mission_late_prob) * ceiling(missionTime/2))
mu_Tf_max<-missionTime-(qnorm(1-mission_late_prob) * 1)

mu_Tf_min<-0
mu_Tf_range<-(mu_Tf_max-mu_Tf_min)

mu_Tf_names<-c('mu_Tf')
mu_Tf_obj<- c(-eps*(1/mu_Tf_range))

#theta_f   - avarage detection on route f
theta_f_max<-(-log(1-beta_f))
theta_f_min<-0
theta_f_range<-(theta_f_max-theta_f_min)
theta_f_names<-c('theta_f')
theta_f_obj<- c(-eps*(1/theta_f_range))

# var_Tf - Total Flight Duration Variance of route f
var_Tf_max<-varTimeRestrict
var_Tf_min<-0
var_Tf_range<-(var_Tf_max-var_Tf_min)
var_Tf_names<-c('var_Tf')
var_Tf_obj<- c(-eps*(1/var_Tf_range))

# # var_If - Total Information Variance of route f
# var_If_max<-varInfRestrict
# var_If_min<-0
# var_If_range<-(var_If_max-var_If_min)
# var_If_names<-c('var_If')
# var_If_obj<- c(-eps*(1/var_If_range))

#sigma_Tf_dummy
sigma_Tf_dummy_names<-c('sigma_Tf_dummy')
sigma_Tf_dummy_obj<- c(0)

#w_u
u_vec<-seq(1,ceiling(missionTime/2),1)
wu_names<-unlist(lapply(u_vec, function (w) paste0('w',",",w)))
wu_obj<-rep(0,length(wu_names))

#All variables - ordered
all_names<-c(xpij_names,yjk_names,wu_names,sigma_Tf_dummy_names,theta_f_names, var_Tf_names,mu_Tf_names)
all_obj<-c(xpij_obj,yjk_obj,wu_obj,sigma_Tf_dummy_obj,theta_f_obj, var_Tf_obj,mu_Tf_obj)

#Constraint Construction
#Constraint 2 - Probability of being late is less than or equal to a given probability
C2_1_xpij<- spMatrix(1,(length(xpij_names)), i=rep(1, (length(xpij_names))), j=(1:(length(xpij_names))), x= c(xpij_Matrix[,4]))
C2_1_yjk<- spMatrix(1,(length(yjk_names)), i=rep(1, (length(yjk_names))), j=(1:(length(yjk_names))), x= c(yjk_Matrix[,3]))
C2_1_wu<-spMatrix(1, length(wu_names))
C2_1_sigma_Tf_dummy<-spMatrix(1, length(sigma_Tf_dummy_names))
C2_1_theta_f<-spMatrix(1, length(theta_f_names))
# C2_1_var_If<-spMatrix(1, length(var_If_names))
C2_1_var_Tf<-spMatrix(1, length(var_Tf_names))
C2_1_mu_Tf<-spMatrix(1, length(mu_Tf_names),i=1,j=1,x=(-1))
C2_1<- cbind(C2_1_xpij,C2_1_yjk,C2_1_wu,C2_1_sigma_Tf_dummy,C2_1_theta_f,C2_1_var_Tf,C2_1_mu_Tf)
C2_1_rhs<-c(0)
C2_1_dir<-c("=")

C2_2_xpij<- spMatrix(1,(length(xpij_names)), i=rep(1, (length(xpij_names))), j=(1:(length(xpij_names))), x= c(xpij_Matrix[,5]))
C2_2_yjk<- spMatrix(1,(length(yjk_names)), i=rep(1, (length(yjk_names))), j=(1:(length(yjk_names))), x= c(yjk_Matrix[,4]))
C2_2_wu<-spMatrix(1, length(wu_names))
C2_2_sigma_Tf_dummy<-spMatrix(1, length(sigma_Tf_dummy_names))
C2_2_theta_f<-spMatrix(1, length(theta_f_names))
# C2_2_var_If<-spMatrix(1, length(var_If_names))
C2_2_var_Tf<-spMatrix(1, length(var_Tf_names),i=1,j=1,x=(-1))
C2_2_mu_Tf<-spMatrix(1, length(mu_Tf_names))
C2_2<- cbind(C2_2_xpij,C2_2_yjk,C2_2_wu,C2_2_sigma_Tf_dummy,C2_2_theta_f,C2_2_var_Tf,C2_2_mu_Tf)
C2_2_rhs<-c(0)
C2_2_dir<-c("=")

C2_3_xpij<- spMatrix(1,(length(xpij_names)))
C2_3_yjk<- spMatrix(1,(length(yjk_names)))
C2_3_wu<-spMatrix(1, length(wu_names))
C2_3_sigma_Tf_dummy<-spMatrix(1, length(sigma_Tf_dummy_names),i=1,j=1,x=qnorm(1-mission_late_prob))
C2_3_theta_f<-spMatrix(1, length(theta_f_names))
# C2_3_var_If<-spMatrix(1, length(var_If_names))
C2_3_var_Tf<-spMatrix(1, length(var_Tf_names))
C2_3_mu_Tf<-spMatrix(1, length(mu_Tf_names),i=1,j=1,x=1)
C2_3<- cbind(C2_3_xpij,C2_3_yjk,C2_3_wu,C2_3_sigma_Tf_dummy,C2_3_theta_f,C2_3_var_Tf,C2_3_mu_Tf)
C2_3_rhs<-c(missionTime)
C2_3_dir<-c("<=")

C2_4_xpij<- spMatrix(1,(length(xpij_names)))
C2_4_yjk<- spMatrix(1,(length(yjk_names)))
C2_4_wu<-spMatrix(1, length(wu_names),i=rep(1,length(wu_names)), j=1:length(wu_names), x=u_vec^2)
C2_4_sigma_Tf_dummy<-spMatrix(1, length(sigma_Tf_dummy_names))
C2_4_theta_f<-spMatrix(1, length(theta_f_names))
# C2_4_var_If<-spMatrix(1, length(var_If_names))
C2_4_var_Tf<-spMatrix(1, length(var_Tf_names),i=1,j=1,x=-1)
C2_4_mu_Tf<-spMatrix(1, length(mu_Tf_names))
C2_4<- cbind(C2_4_xpij,C2_4_yjk,C2_4_wu,C2_4_sigma_Tf_dummy,C2_4_theta_f,C2_4_var_Tf,C2_4_mu_Tf)
C2_4_rhs<-c(0)
C2_4_dir<-c(">=")

C2_5_xpij<- spMatrix(1,(length(xpij_names)))
C2_5_yjk<- spMatrix(1,(length(yjk_names)))
C2_5_wu<-spMatrix(1, length(wu_names),i=rep(1,length(wu_names)), j=1:length(wu_names), x=u_vec)
C2_5_sigma_Tf_dummy<-spMatrix(1, length(sigma_Tf_dummy_names),i=1,j=1,x=-1)
C2_5_theta_f<-spMatrix(1, length(theta_f_names))
# C2_5_var_If<-spMatrix(1, length(var_If_names))
C2_5_var_Tf<-spMatrix(1, length(var_Tf_names))
C2_5_mu_Tf<-spMatrix(1, length(mu_Tf_names))
C2_5<- cbind(C2_5_xpij,C2_5_yjk,C2_5_wu,C2_5_sigma_Tf_dummy,C2_5_theta_f,C2_5_var_Tf,C2_5_mu_Tf)
C2_5_rhs<-c(0)
C2_5_dir<-c("=")

C2_6_xpij<- spMatrix(1,(length(xpij_names)))
C2_6_yjk<- spMatrix(1,(length(yjk_names)))
C2_6_wu<-spMatrix(1, length(wu_names),i=rep(1,length(wu_names)), j=1:length(wu_names), x=rep(1,length(wu_names)))
C2_6_sigma_Tf_dummy<-spMatrix(1, length(sigma_Tf_dummy_names))
C2_6_theta_f<-spMatrix(1, length(theta_f_names))
# C2_6_var_If<-spMatrix(1, length(var_If_names))
C2_6_var_Tf<-spMatrix(1, length(var_Tf_names))
C2_6_mu_Tf<-spMatrix(1, length(mu_Tf_names))
C2_6<- cbind(C2_6_xpij,C2_6_yjk,C2_6_wu,C2_6_sigma_Tf_dummy,C2_6_theta_f,C2_6_var_Tf,C2_6_mu_Tf)
C2_6_rhs<-c(1)
C2_6_dir<-c("=")

C2<-rbind(C2_1,C2_2,C2_3,C2_4,C2_5,C2_6)
C2_rhs<-rbind(C2_1_rhs,C2_2_rhs,C2_3_rhs,C2_4_rhs,C2_5_rhs,C2_6_rhs)
C2_dir<-rbind(C2_1_dir,C2_2_dir,C2_3_dir,C2_4_dir,C2_5_dir,C2_6_dir)


#Constraint 3, probability of number of decetion is larger than 1 must be smaller than or equal to a given probability
C3_xpij<- spMatrix(1,(length(xpij_names)), i=rep(1, (length(xpij_names))), j=(1:(length(xpij_names))), x= c(xpij_Matrix[,6]))
C3_yjk<- spMatrix(1,(length(yjk_names)), i=rep(1, (length(yjk_names))), j=(1:(length(yjk_names))), x= c(yjk_Matrix[,6]))
C3_wu<-spMatrix(1, length(wu_names))
C3_sigma_Tf_dummy<-spMatrix(1, length(sigma_Tf_dummy_names))
C3_theta_f<-spMatrix(1, length(theta_f_names),i=1,j=1,x=(-1))
# C3_var_If<-spMatrix(1, length(var_If_names))
C3_var_Tf<-spMatrix(1, length(var_Tf_names))
C3_mu_Tf<-spMatrix(1, length(mu_Tf_names))
C3<- cbind(C3_xpij,C3_yjk,C3_wu,C3_sigma_Tf_dummy,C3_theta_f,C3_var_Tf,C3_mu_Tf)
C3_rhs<-c(0)
C3_dir<-c("=")

#Upper bound of theta is defined when defining the upper bounds of the variables of the model

#Constraint 4 - variance restriction is defined when defining the upper bounds of the variables of the model

# #Constraint 5, calculates the variance of the information collected 
# C5_xpij<- spMatrix(1,(length(xpij_names)))
# C5_yjk<- spMatrix(1,(length(yjk_names)), i=rep(1, (length(yjk_names))), j=(1:(length(yjk_names))), x= c(yjk_Matrix[,6]))
# C5_wu<-spMatrix(1, length(wu_names))
# C5_sigma_Tf_dummy<-spMatrix(1, length(sigma_Tf_dummy_names))
# C5_theta_f<-spMatrix(1, length(theta_f_names))
# # C5_var_If<-spMatrix(1, length(var_If_names),i=1,j=1,x=(-1))
# C5_var_Tf<-spMatrix(1, length(var_Tf_names))
# C5_mu_Tf<-spMatrix(1, length(mu_Tf_names))
# C5<- cbind(C5_xpij,C5_yjk,C5_wu,C5_sigma_Tf_dummy,C5_theta_f,C5_var_Tf,C5_mu_Tf)
# C5_rhs<-c(0)
# C5_dir<-c("=")

#5.2. restriction for the variance of the information collection is defined when defining the upper bounds of the variables of the model
#Constraint 6 - Number of collections at a node must be smaller than or equal to the number of times the node appears on the route

C6<-do.call("rbind",sapply(target_set, function (a){
  C6_xpij<- spMatrix(1,(length(xpij_names)))
  
  temp_6_x<-which(xpij_Matrix[,3]==a)
  temp_6_y<-which(yjk_Matrix[,1]==a)
  
  C6_xpij<- spMatrix(1, length(xpij_names), i=rep(1,length(temp_6_x)), j=temp_6_x, x=rep(-1, length(temp_6_x)))
  C6_yjk<- spMatrix(1,(length(yjk_names)), i=rep(1, length(temp_6_y)), j=temp_6_y, x= rep(1, length(temp_6_y)))
  C6_wu<-spMatrix(1, length(wu_names))
  C6_sigma_Tf_dummy<-spMatrix(1, length(sigma_Tf_dummy_names))
  C6_theta_f<-spMatrix(1, length(theta_f_names))
  # C6_var_If<-spMatrix(1, length(var_If_names))
  C6_var_Tf<-spMatrix(1, length(var_Tf_names))
  C6_mu_Tf<-spMatrix(1, length(mu_Tf_names))
  C6_temp<- cbind(C6_xpij,C6_yjk,C6_wu,C6_sigma_Tf_dummy,C6_theta_f,C6_var_Tf,C6_mu_Tf)
  
  return(C6_temp)
  
}))

C6_rhs<-rep(0,length(target_set))
C6_dir<-rep("<=",length(target_set))


#Constraint 7 - Defines the revisits. Target j must be visited before a revisit

#temproray matrix to generate order of yjk variables
temp7<-(length(target_set)*(length(allowed_revisit_set)-1)) #number of constraints
temp7_2<-cbind(1:(length(yjk_names)-1), 2:length(yjk_names))
temp7_2<-temp7_2[-seq(length(allowed_revisit_set),length(yjk_names)-length(allowed_revisit_set),by=length(allowed_revisit_set)),]

C7_xpij<- spMatrix(temp7, length(xpij_names))
C7_yjk<- spMatrix(temp7,length(yjk_names), i=rep(1:temp7,each=(length(allowed_revisit_set)-1)), j=as.vector(t(temp7_2)), x=rep(c(1,-1),temp7))
C7_wu<-spMatrix(temp7, length(wu_names))
C7_sigma_Tf_dummy<-spMatrix(temp7, length(sigma_Tf_dummy_names))
C7_theta_f<-spMatrix(temp7, length(theta_f_names))
# C7_var_If<-spMatrix(temp7, length(var_If_names))
C7_var_Tf<-spMatrix(temp7, length(var_Tf_names))
C7_mu_Tf<-spMatrix(temp7, length(mu_Tf_names))
C7<- cbind(C7_xpij,C7_yjk,C7_wu,C7_sigma_Tf_dummy,C7_theta_f,C7_var_Tf,C7_mu_Tf)

C7_rhs<-rep(0,temp7)
C7_dir<-rep(">=",temp7)

#Constraint 8 -  Flow balance equations --> if there is an incoming arc to a target, there has to be an outgoing arc as well
C8_xpij<-do.call("rbind",lapply(head(tail(time_step_set,-1),-1), function (p){
  
  C8_xpij_temp<-do.call("rbind",lapply(target_set, function (j){
    
    # xpij_Matrix[temp8_1,]
    # xpij_Matrix[temp8_2,]
    
    temp8_1<-which(xpij_Matrix[,1]==p & xpij_Matrix[,3]==j) #incoming arcs to j at p
    temp8_2<-which(xpij_Matrix[,1]==(p+1) & xpij_Matrix[,2]==j) #outgoing arcs from j at p+1
    
    C8_xpij_1<-spMatrix(1, length(xpij_names), i=rep(1,length(temp8_1)), j=temp8_1, x=rep(1,length(temp8_1))) #incoming arcs to j at p
    C8_xpij_2<-spMatrix(1, length(xpij_names), i=rep(1,length(temp8_2)), j=temp8_2, x=rep(-1,length(temp8_2))) #outgoing arcs from j at p+1
    
    C8_xpij_j<-(C8_xpij_1 + C8_xpij_2)
    
    return(C8_xpij_j)
  }))
  
  
  return(C8_xpij_temp)
  
  
}))

C8_yjk<- spMatrix(nrow(C8_xpij),length(yjk_names))
C8_wu<-spMatrix(nrow(C8_xpij), length(wu_names))
C8_sigma_Tf_dummy<-spMatrix(nrow(C8_xpij), length(sigma_Tf_dummy_names))
C8_theta_f<-spMatrix(nrow(C8_xpij), length(theta_f_names))
# C8_var_If<-spMatrix(nrow(C8_xpij), length(var_If_names))
C8_var_Tf<-spMatrix(nrow(C8_xpij), length(var_Tf_names))
C8_mu_Tf<-spMatrix(nrow(C8_xpij), length(mu_Tf_names))
C8<- cbind(C8_xpij,C8_yjk,C8_wu,C8_sigma_Tf_dummy,C8_theta_f,C8_var_Tf,C8_mu_Tf)

C8_rhs<-rep(0,nrow(C8_xpij))
C8_dir<-rep("=",nrow(C8_xpij))

#Flow balance equations
# xpij_Matrix[which(xpij_Matrix[,1]==1),]
#Constraint 9: flow balance for time period 1 and 2
C9_xpij_1<-spMatrix(n_targets, length(xpij_names), i=(1:n_targets), j=which(xpij_Matrix[,1]==1), x=rep(1,n_targets))
C9_xpij_2<-do.call("rbind",lapply(target_set,function(s){
  spMatrix(1, length(xpij_names), i=rep(1,(n_targets+1)), j=which(xpij_Matrix[,1]==2 & xpij_Matrix[,2]==s ), x=rep(-1,(n_targets+1)))
}))
C9_xpij<- C9_xpij_1 + C9_xpij_2
C9_yjk<- spMatrix(n_targets,length(yjk_names))
C9_wu<-spMatrix(n_targets, length(wu_names))
C9_sigma_Tf_dummy<-spMatrix(n_targets, length(sigma_Tf_dummy_names))
C9_theta_f<-spMatrix(n_targets, length(theta_f_names))
# C9_var_If<-spMatrix(n_targets, length(var_If_names))
C9_var_Tf<-spMatrix(n_targets, length(var_Tf_names))
C9_mu_Tf<-spMatrix(n_targets, length(mu_Tf_names))
C9<- cbind(C9_xpij,C9_yjk,C9_wu,C9_sigma_Tf_dummy,C9_theta_f,C9_var_Tf,C9_mu_Tf)

C9_rhs<-rep(0,n_targets)
C9_dir<-rep("=",n_targets)

#Constraint 10: flow balance for time period 1-must leave
C10_xpij<-spMatrix(1, length(xpij_names), i=rep(1,n_targets), j=which(xpij_Matrix[,1]==1), x=rep(1,n_targets))
C10_yjk<- spMatrix(1,length(yjk_names))
C10_wu<-spMatrix(1, length(wu_names))
C10_sigma_Tf_dummy<-spMatrix(1, length(sigma_Tf_dummy_names))
C10_theta_f<-spMatrix(1, length(theta_f_names))
# C10_var_If<-spMatrix(1, length(var_If_names))
C10_var_Tf<-spMatrix(1, length(var_Tf_names))
C10_mu_Tf<-spMatrix(1, length(mu_Tf_names))
C10<- cbind(C10_xpij,C10_yjk,C10_wu,C10_sigma_Tf_dummy,C10_theta_f,C10_var_Tf,C10_mu_Tf)
C10_rhs<-c(1)
C10_dir<-c("=")

#Constraint 11: flow balance for time period 1-must leave
temp11<-which(xpij_Matrix[,3]==n_nodes)
C11_xpij<-spMatrix(1, length(xpij_names), i=rep(1,length(temp11)), j=temp11, x=rep(1,length(temp11)))
C11_yjk<- spMatrix(1,length(yjk_names))
C11_wu<-spMatrix(1, length(wu_names))
C11_sigma_Tf_dummy<-spMatrix(1, length(sigma_Tf_dummy_names))
C11_theta_f<-spMatrix(1, length(theta_f_names))
# C11_var_If<-spMatrix(1, length(var_If_names))
C11_var_Tf<-spMatrix(1, length(var_Tf_names))
C11_mu_Tf<-spMatrix(1, length(mu_Tf_names))
C11<- cbind(C11_xpij,C11_yjk,C11_wu,C11_sigma_Tf_dummy,C11_theta_f,C11_var_Tf,C11_mu_Tf)
C11_rhs<-c(1)
C11_dir<-c("=")

#Constraint 12 : Limit reccurrenct at nodes to K-1
C12_xpij<-do.call("rbind",lapply(target_set,function(r){
  temp12<-which(xpij_Matrix[,2]==r & xpij_Matrix[,3]==r)
  spMatrix(1, length(xpij_names), i=rep(1,length(temp12)), j=temp12, x=rep(1,length(temp12)))
}))
C12_yjk<- spMatrix(n_targets,length(yjk_names))
C12_wu<-spMatrix(n_targets, length(wu_names))
C12_sigma_Tf_dummy<-spMatrix(n_targets, length(sigma_Tf_dummy_names))
C12_theta_f<-spMatrix(n_targets, length(theta_f_names))
# C12_var_If<-spMatrix(n_targets, length(var_If_names))
C12_var_Tf<-spMatrix(n_targets, length(var_Tf_names))
C12_mu_Tf<-spMatrix(n_targets, length(mu_Tf_names))
C12<- cbind(C12_xpij,C12_yjk,C12_wu,C12_sigma_Tf_dummy,C12_theta_f,C12_var_Tf,C12_mu_Tf)
C12_rhs<-rep((n_revisits-1),n_targets)
C12_dir<-rep("<=",n_targets)

# all_names<-c(xpij_names,yjk_names,wu_names,sigma_Tf_dummy_names,theta_f_names, var_If_names,var_Tf_names,mu_Tf_names)

# Build model
model <- list()
model$modelname <- 'UAV Prize Collection'
model$modelsense <- 'max'

# initialize data for variables
model$lb       <- 0
model$ub       <- c(rep(1, length(xpij_names)),rep(1, length(yjk_names)), rep(1, length(wu_names)),ceiling(missionTime/2),(-log(1-beta_f)),varTimeRestrict,mu_Tf_max)
model$vtype    <- c(rep("B", length(xpij_names)),rep("B", length(yjk_names)), rep("B", length(wu_names)),"C","C","C","C")
model$obj      <- all_obj
model$varnames <- all_names

# build constraint matrix
model$A        <- rbind(C2,C3,C6,C7,C8,C9,C10,C11,C12)
model$rhs      <- c(C2_rhs,C3_rhs,C6_rhs,C7_rhs,C8_rhs,C9_rhs,C10_rhs,C11_rhs,C12_rhs)
model$sense    <- c(C2_dir,C3_dir,C6_dir,C7_dir,C8_dir,C9_dir,C10_dir,C11_dir,C12_dir)

# gurobi_write(model, 'C:/Users/Erdi Dasdemir/OneDrive/Belgeler/Erdi Research/((Erdi Ph.D. Thesis/(2019) Rajan Batta/UAV Problem/Codes/UAV.mps')
#---------------------------------------------------------------------------------------------------------------

# set parameters
params <- list()
#params$TimeLimit<-1800
# params$MIPGap<-0
# params$MIPFocus<-2

ptm<-proc.time()
result <- gurobi(model,params)
runTime<-(proc.time()-ptm)
runTime<-sum(runTime[1:2])
# 
# print(result$objval)
# # 
decvr<-result$x
#all_names[which(decvr>0.0001)]
#decvr[which(decvr>0.0001)]

#(result$objval)/10000


ExpectedInfCollect<-(result$objval+(-1*(tail(decvr,3)%*% c(theta_f_obj, var_Tf_obj,mu_Tf_obj))))/1000

# xp,i,j results
xpij_results<-decvr[1:length(xpij_names)]
xpij_variables<-all_names[1:length(xpij_names)]

xpij_variables<-xpij_variables[which(xpij_results>0.0001)]
xpij_results<-xpij_results[which(xpij_results>0.0001)]

#yjk results
yjk_results<-decvr[(length(xpij_names)+1):(length(xpij_names)+length(yjk_names))]
yjk_variables<-all_names[(length(xpij_names)+1):(length(xpij_names)+length(yjk_names))]
# 
yjk_variables<-yjk_variables[which(yjk_results>0.0001)]
yjk_results<-yjk_results[which(yjk_results>0.0001)]

#theta_f_names, var_If_names,var_Tf_names,mu_Tf_names
# other_results<-tail(decvr,4)
other_results<-tail(decvr,3)[c(3,1,2)]


cat(c(missionTime,beta_f, varTimeRestrict, ExpectedInfCollect, other_results, result$objval, result$objbound,result$mipgap,result$runtime, runTime), file = "2_Demonstration_Summary.txt", append = TRUE)
cat("\n", file ="2_Demonstration_Summary.txt", append = TRUE)


sink("2_Demonstration_DecSpace.txt", append=TRUE)
cat("Restrictions(Mission Time, Risk (Beta), Time Variance):", c(missionTime,beta_f,varTimeRestrict))
cat("\n")
cat("----------------------------------------")
cat("\n")
cat("Status:",result$status )
cat("\n")
cat("Optimality gap:",(result$mipgap)*100)
cat("\n")
cat("Run Time given by Gurobi:", result$runtime)
cat("\n")
cat("Run Time given by R:", runTime)
cat("\n")
cat("Objective value:",result$objval)
cat("\n")
cat("Best available upper bound:",result$objbound)
cat("\n")
cat("Expected Inf Collection:",ExpectedInfCollect)
cat("\n")
cat("Other Variables:",other_results)
cat("\n")
cat("xpij Result: ",  xpij_variables)
cat("\n")
cat("xpij Result: ",xpij_results)
cat("\n")
cat("yjk Result: ",  yjk_variables)
cat("\n")
cat("yjk Result: ",yjk_results)
cat("\n")
cat("\n")
cat("\n")
sink()


























#---------------------------------------------------------------------------------------------------------------