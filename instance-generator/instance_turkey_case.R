# #Load Packages
# library(Matrix)
library(dplyr)
library(reshape2)
library(jpeg)
library(plotrix)
library(grDevices)

#Set Global Parameters
options(max.print = 99999)
set.seed(594)

# read data
target_data <- as.data.frame(read.table("turkey_case_input/turkey_data.txt",header = TRUE))
# target_radius <- 80
target_radius <- target_data$border_length
target_data["target_area"] <- pi * target_radius^2


# problem Parameters
n_targets <- nrow(target_data) - 1  # number of targets excluding origin and destination
n_nodes <- nrow(target_data)  # number of nodes including origin and destination
discount_ratio <- 30  # maximum discount in terms of %
flight_v <- 360  # flight speed (km per hour)
sensor_range <- 80

# search time reduction parameter
alpha_time_reduction <- 0.8

#search and collection duration
s_t <- 50/60
c_t <- 10/60
search_duration <- s_t + c_t

#effectiveness of the search and collection sensors
e_s <- (1 - exp(-(flight_v*sensor_range)/((target_data$target_area)))^s_t) #effectiveness of search sensor - by Xia's paper--28km is based on Global Hawk
e_c <- 0.5 # effectiveness of collection sensor - by Moskal's previous papers
target_data["search_effectiveness"] <- e_s
target_data["collection_effectiveness"] <- e_c

target_data["expected_collection_at_first_visit"] <- target_data$search_effectiveness * target_data$collection_effectiveness * target_data$info_prob
e_s <- tail(e_s, -1)

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
plot(c(), xlim=c(25,2000), ylim=c(0, 875), axes=T, xlab="", ylab="", xaxt="n", yaxt="n" ) # see ?plot.function
jp <- readJPEG('turkey_case_input/map_blank2.jpg')
rasterImage(jp, xleft=-115, xright=2100, ybottom=-35, ytop=880)
box()
points(x = filter(target_data, target_id == 0)$x,y=filter(target_data, target_id == 0)$y,col = "black", pch=17,cex = 2.5)
text(x = filter(target_data, target_id == 0)$x+50,y=filter(target_data, target_id == 0)$y+20,labels = "h",col = "black",cex = 1.5)

for (i in tail(target_data$target_id,-1)) {
  
  coord_now <- filter(target_data, target_id==i)
  points(x = coord_now$x-5, y=coord_now$y, col = "black", pch = 215,cex = 2.5, lwd = 4) 
  draw.circle(coord_now$x,coord_now$y,coord_now$border_length, lty=2,border = "black", col = adjustcolor("grey40", alpha.f = 0.2), lwd=2 ) #fena degil
  
  
  text(x = coord_now$x - 85,y = coord_now$y-55,labels = i,col = "black",cex = 2.5)
  
}

legend("top", inset = c(-0.003,-0.11), legend = c("base", "target center", "target search area"), xpd = TRUE, 
       horiz = TRUE,  col=c("black","black","black"),pch = c(17, 215, 1), bty = "n", pt.cex=2.5, text.width=c(240,430,50))



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
  pair_from_coord <- filter(target_data, target_id == pair_from_id)
  pair_to_coord <- filter(target_data, target_id == pair_to_id)
  sqrt((pair_from_coord$x - pair_to_coord$x)^2 + (pair_from_coord$y - pair_to_coord$y)^2)
})

pair_matrix <- cbind(pair_matrix, round(euc_distances/flight_v,4))
trajectory_df <- as.data.frame(pair_matrix)
colnames(trajectory_df) <- c('from_node', "to_node", "travel_duration")



# Update distances with discounted euclidean distances. Discount rates linearly increase from 0 to discount_ratio (%30 in the paper)
discount_rates <- unlist(lapply(pair_matrix[,3],function (x) (discount_ratio/(max(pair_matrix[,3]) - min(pair_matrix[,3])))*(x - min(pair_matrix[,3]))))
discount_amounts <- (pair_matrix[,3] * discount_rates)/100
pair_matrix[,3] <- (pair_matrix[,3] - discount_amounts)

x_disc_rate <- cbind(pair_matrix[,1:2], discount_rates)
colnames(x_disc_rate) <- NULL


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
prob_inf_exists <- tail(target_data$info_prob, -1)
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
detect_rates <- round(tail(target_data$detection_rate, -1), 4) #number of detection per 1 hour flight
lamda_j <- c(0, as.vector(t(detect_rates)))
lamda_base <- 0.001

# radar exposure when visiting targets
target_df['number_of_detections'] <- target_df$search_duration * rep((tail(lamda_j, -1) + lamda_base), n_revisits)
trajectory_df['number_of_detections'] <- trajectory_df$travel_duration * lamda_base


target_df
trajectory_df