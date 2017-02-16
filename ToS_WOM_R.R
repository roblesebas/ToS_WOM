setwd("~/Google Drive/DOCTORADO/PAPERS/G. OBJECTIVE 2 - Real Data (ToS)/Complex contagion paper/ToS_WOM") # Mac
setwd("C:/Users/Sebastian/Google Drive/DOCTORADO/PAPERS/G. OBJECTIVE 2 - Real Data (ToS)/Complex contagion paper/ToS_WOM") # Asus
setwd("C:/Users/sebas/Google Drive/DOCTORADO/PAPERS/G. OBJECTIVE 2 - Real Data (ToS)/Complex contagion paper/ToS_WOM") # German

library(igraph)
library(stringr)
library(sqldf)
library(ggplot2)
library(reshape2)
library("plyr")
library(cluster)
library(mclust)
library(psych)
library(car)
library(gpairs)
library(cluster) 
library(poLCA)
library(lavaan)
library(semPlot)
library(corrplot)
library(multcomp)
library(reshape2)
library(car)
library(gplots)
library(lattice)
library(vcd)
require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)
library(data.table)

#### Connect to database ####
db <- dbConnect(SQLite(), dbname="EM_NM.sqlite")
user <- dbReadTable(db, "user")
invitation <- dbReadTable(db, "invitation")
usage <- dbReadTable(db, "usage")

# Another alternative... database sometimes do not have data!
user <- read.csv("user_10.csv")
invitation <- read.csv("invitations_2.csv")
usage <- read.csv("usage.csv")
network <- read.graph("network.gml", format ="gml")
net.act <- read.csv("net_act.csv")

# With this information create the networks
network <- graph.data.frame(invitation, directed = TRUE, user)
network_act <- induced_subgraph(network, V(network)[adoption == 1])

write.graph(network, "network.gml", "gml")
write.graph(network_act, "network_act.gml", "gml")


#### Getting data Read files and create the network ####

user_python <- read.csv("user.csv")
user_python$X <- NULL
user_python$invitation_date <- as.numeric(user_python$invitation_date)
user_python$activation <- as.numeric(user_python$activation)
invitation_python <- read.csv("invitation.csv")
invitation_python$id <- NULL
invitation_python$invitation_date <- as.numeric(invitation_python$invitation_date)

network <- graph.data.frame(invitation_python, directed = TRUE, user_python )

#### Adding new variables to user ####

## Add WOM_invitation to invitation df

invitation$WOM_invitation <- ifelse(invitation$Source %in% c("srobledog@unal.edu.co",
                                                             "martha.zuluaga@ucaldas.edu.co",
                                                             "tos_man@unal.edu.co"), 0,1)
invitation$WOM_invitation <- factor(invitation$WOM_invitation, 
                                    levels = c(0,1),
                                    labels = as.character("Init_inv", "WOM_inv"))

## Add WOM_day
# Option 3

WOM_day_df <- data.frame(id = character(), WOM_day = numeric(), 
                         stringsAsFactors = FALSE)
for (i in unique(invitation$Source)) {
  mydata_1.0 <- invitation[invitation$Source %in% i,c(2:3)]
  users_last_level <- data.frame(id =user[user$id %in% c("martha.zuluaga@ucaldas.edu.co",
                                                         "srobledog@unal.edu.co",
                                                         "tos_man@unal.edu.co"),"id"])
  WOM_day_1 <- 0
  WOM_count <- 1
  while(WOM_count > 0){
    users_last_level <- rbind(users_last_level, i)
    names(mydata_1.0) <- c("WOM_Target", "WOM_invitation_date")
    mydata_1.1 <- merge(mydata_1.0, invitation, by.x="WOM_Target", 
                        by.y = "Source", all.x = TRUE )
    mydata_1.2 <- mydata_1.1[complete.cases(mydata_1.1)==TRUE,]
    mydata_1 <- mydata_1.2[mydata_1.2$WOM_invitation_date == mydata_1.2$invitation_date,]
    WOM_count <- as.numeric(nrow(mydata_1)) # if WOM_day > 0 continue the process, stop
    WOM_day_1 <-  WOM_day_1 + WOM_count
    mydata <- mydata_1[!(mydata_1$WOM_Target %in% users_last_level$id), ]
    mydata_1.0 <- mydata[,c("Target", "invitation_date")]
  } 
  new_rows <- data.frame(cbind(id = i, WOM_day = WOM_day_1))
  WOM_day_df <- rbind(WOM_day_df, new_rows)
  WOM_day_df$WOM_day <- as.numeric(WOM_day_df$WOM_day)
}

user_1 <- user 
user_1 <- merge(user, WOM_day_df[,c(1:2)], by = "id", all.x = TRUE)
user_1$WOM_day[is.na(user_1$WOM_day)] <- 0
user_1 <- user_1[,c(1:16, 26,17:25)]
user <- user_1

# Example...

x <- 10
while(x > 0) {
  x <- x-1
  print(x)
}

y=0
while(y <5){ print( y<-y+1) }

# Option 2

mydata_1.0 <- invitation[invitation$Source == "srobledog@unal.edu.co",c(2:3)]
dummy <- invitation[invitation$Source %in% c("martha.zuluaga@ucaldas.edu.co",
                                             "srobledog@unal.edu.co", 
                                             "tos_man@unal.edu.co",
                                             "jczuluagag@unal.edu.co"),c(1:3)]

# Step 1: Organize data

names(mydata_1.0) <- c("WOM_Target", "WOM_invitation_date")

# create mydata_1 With all data
mydata_1.1 <- merge(mydata_1.0, invitation, by.x="WOM_Target", 
                    by.y = "Source", all.x = TRUE )

# Only complete cases
mydata_1.2 <- mydata_1.1[complete.cases(mydata_1.1)==TRUE,]
mydata_1 <- mydata_1.2[mydata_1.2$WOM_invitation_date == mydata_1.2$invitation_date,]
WOM_count <- as.numeric(nrow(mydata_1)) # if WOM_day > 0 continue the process, stop
WOM_day_1 <-  WOM_count

# Step 2: Organize data
mydata_2.0 <- mydata_1[,c("Target", "invitation_date")]
names(mydata_2.0) <- c("WOM_Target", "WOM_invitation_date")

# create mydata_2 With all data
mydata_2.1 <- merge(mydata_2.0, invitation, by.x = "WOM_Target", 
                    by.y = "Source", all.x = TRUE)

# Only complete cases
mydata_2.2 <- mydata_2.1[complete.cases(mydata_2.1)==TRUE,]
mydata_2 <- mydata_2.2[mydata_2.2$WOM_invitation_date == mydata_2.2$invitation_date,]
WOM_count <- as.numeric(nrow(mydata_1)) # if WOM_count > 0 continue the process, stop
WOM_day_2 <- WOM_day_1 + WOM_count 

# Step 3: Organize data
mydata_3.0 <- mydata_2[,c("Target", "invitation_date")]
names(mydata_3.0) <- c("WOM_Target", "WOM_invitation_date")

# create mydata_2 With all data
mydata_3.1 <- merge(mydata_3.0, invitation, by.x = "WOM_Target", 
                    by.y = "Source", all.x = TRUE)

# Only complete cases
mydata_3.2 <- mydata_3.1[complete.cases(mydata_3.1)==TRUE,]
mydata_3 <- mydata_3.2[mydata_3.2$WOM_invitation_date == mydata_3.2$invitation_date,] 
# Also past users (links)
WOM_count <- as.numeric(nrow(mydata_3)) # if WOM_count > 0 continue the process, stop
WOM_day_3 <- WOM_day_2 + WOM_count 

# Step 4: Organize data
mydata_4.0 <- mydata_3[,c("Target", "invitation_date")]
names(mydata_4.0) <- c("WOM_Target", "WOM_invitation_date")

# create mydata_2 With all data
mydata_4.1 <- merge(mydata_4.0, invitation, by.x = "WOM_Target", 
                    by.y = "Source", all.x = TRUE)

# Only complete cases
mydata_4.2 <- mydata_4.1[complete.cases(mydata_4.1)==TRUE,]
mydata_4 <- mydata_4.2[mydata_4.2$WOM_invitation_date == mydata_4.2$invitation_date,] 
# Also past users (links)
WOM_count <- as.numeric(nrow(mydata_4)) # if WOM_count > 0 continue the process, stop
WOM_day_4 <- WOM_day_3 + WOM_count 

# Step 4: Organize data
mydata_5.0 <- mydata_4[,c("Target", "invitation_date")]
names(mydata_5.0) <- c("WOM_Target", "WOM_invitation_date")


# create mydata_2 With all data
mydata_5.1 <- merge(mydata_5.0, invitation, by.x = "WOM_Target", 
                    by.y = "Source", all.x = TRUE)

# Only complete cases
mydata_5.2 <- mydata_5.1[complete.cases(mydata_5.1)==TRUE,]
mydata_5 <- mydata_5.2[mydata_5.2$WOM_invitation_date == mydata_5.2$invitation_date,] 
# Also past users (links)
WOM_count <- as.numeric(nrow(mydata_5)) # if WOM_count > 0 continue the process, stop
WOM_day_5 <- WOM_day_4 + WOM_count



# Option 1
# We need to select one day for example 7
day <- 2
day <- unique(dummy_1[,"invitation_date"])

user_target <- dummy_1[ dummy_1$invitation_date == day,c("Target", "invitation_date") ]
names(user_target) <- c("WOM_1", "day")
# We need to select one of the invitation users. Another for loop
user_target_1 <- user_target[user_target$WOM_1 == "almarinfl@unal.edu.co",]
# Merge user_target_1 with invitation to identify the invitations
user_wom <- merge(user_target_1, invitation, by.x = "WOM_1", by.y = "Source", all.x = TRUE)
WOM_user_day <- user_wom[complete.cases(user_wom)==TRUE,]
# Select the rows that invitation_date is equal to day
WOM_user_day_1 <- WOM_user_day[WOM_user_day$day == WOM_user_day$invitation_date,]
WOM_day <- as.numeric(nrow(WOM_user_day_1))
# How many of Target send invitations in day day?
names(WOM_user_day_1) <- c("WOM_1", "initial_day","WOM_Target", "WOM_invitation_date")
WOM_user_day_2 <- merge(WOM_user_day_1, invitation, by.x = "WOM_Target", by.y="Source", all.x = TRUE)
WOM_user_day_3 <- WOM_user_day_2[complete.cases(WOM_user_day_2)==TRUE,]
WOM_user_day_4 <- WOM_user_day_3[WOM_user_day_3$initial_day == WOM_user_day_3$invitation_date,]
WOM_day <- WOM_day + as.numeric(nrow(WOM_user_day_4))

## Add usage_days

usage_days_1 <- usage[,c("User", "activation_date")]
usage_days_2 <- setDT(usage_days_1)[,.(count=uniqueN(activation_date)), by = User]
names(usage_days_2) <- c("id", "usage_days")
user <- merge(user, usage_days_2, by = "id", all.x = TRUE)
user$usage_days[is.na(user$usage_days)] <- 0
user <- user[,c(1:10,24,11:23)]

## Add sender type to links  

invitation_1 <- merge(invitation, user[,c("id", "net_actor_forw"),], by.x= "Source", by.y = "id", all.x = TRUE)
invitation_1$influencer <- ifelse(invitation_1$net_actor_forw == "influencer", 1, 0)
invitation_1$strong_forw <- ifelse(invitation_1$net_actor_forw == "strong_forwarding", 1, 0)
invitation_1$low_forw <- ifelse(invitation_1$net_actor_forw == "low_forwarding", 1, 0)
invitation_1$initiator <- ifelse(invitation_1$net_actor_forw == "initiator", 1, 0)

invitation_2 <- invitation_1[,c("Target", "influencer", "strong_forw", "low_forw","initiator")]
invitation_2 <- ddply(invitation_2, ~Target, summarise, influencer=sum(influencer), strong_forw=sum(strong_forw), 
                      low_forw=sum(low_forw),initiator=sum(initiator) )
user_0 <- user[,c(1:21)]
user_1 <- merge(user_0, invitation_2, by.x = "id", by.y = "Target", all.x = TRUE)
user_1[user_1$id == "srobledog@unal.edu.co", c("influencer", "strong_forw", "low_forw","initiator")] <- 0
write.csv(user_1, "user_9.csv", row.names = FALSE)
write.csv(invitation_2[,c(1:4)], "invitations_1.csv", row.names = FALSE)
user <- user_1


## Create user.forw.t 
# Transforming and standardizing the data

autoTransform <- function(x) {
  library(forecast)
  return(scale(BoxCox(x, BoxCox.lambda(x))))
}

user.forw.raw <- user[user$net_actor == "forwarding",]
user.forw <- user.forw.raw[,c(7:15)]

user.forw.t <- user.forw.raw # t for dummy
user.forw.t$trans <- autoTransform(user.forw.t$invitations_send)


user.forw.t$forw_type <- ifelse(user.forw.t$trans <= 1, "low_forwarding",
                                ifelse(user.forw.t$trans > 1 & user.forw.t$trans <=  2 , 
                                       "strong_forwarding", "influencer"))

table(user.forw.t$forw_type)
table(user.forw.t[user.forw.t$forw_type == "low_forwarding", c( "invitations_send")])
table(user.forw.t[user.forw.t$forw_type == "strong_forwarding", c( "invitations_send")])
table(user.forw.t[user.forw.t$forw_type == "influencer", c( "invitations_send")])

hist(user.forw.t[user.forw.t$forw_type == "influencer", c( "invitations_send")], main = "Hist. Influencer vs invitations_send", xlab = "", col = "black")
hist(user.forw.t[user.forw.t$forw_type == "strong_forwarding", c( "invitations_send")], main = "Hist. strong_forwarding vs invitations_send", xlab = "", col = "black")
hist(user.forw.t[user.forw.t$forw_type == "low_forwarding", c( "invitations_send")], main = "Hist. low_forwarding vs invitations_send", xlab = "", col = "black")

table(user$inv_received)

## Add forwarding type to user

user$net_actor_forw <- NULL
user$forw_type <- NULL
user_1 <- merge(user, user.forw.t[,c("id", "forw_type")], by = "id", all.x = TRUE)
user_1$net_actor_forw <- ifelse(user_1$net_actor == "forwarding", user_1$forw_type, user_1$net_actor)
user_1$net_actor_forw <- ifelse(user_1$net_actor_forw == 2, "initiator", 
                                ifelse(user_1$net_actor_forw == 3, "not_active", 
                                       ifelse(user_1$net_actor_forw == 4, "not_forwarding", user_1$net_actor_forw)))
user <- user_1
write.csv(user, "user_7.csv", row.names = FALSE)


## Add WOM_effect

user.forw <- user[user$net_actor == "forwarding", ]
user.forw.id <- user[user$net_actor == "forwarding", c("id", "invitations_send")]
user.forw.inv <- merge(user.forw.id, invitation,by.x = "id", 
                       by.y = "Source", all.x = TRUE )
user.forw.inved <- merge(user.forw.inv, invitation, by.x="Target",
                         by.y = "Source", all.x = TRUE)
colnames(user.forw.inved) <- c("inv", "id", "invitations_send", "invitation_date_inv", "wom", "invitation_date_wom")
user.forw.inved.no_na <- na.omit(user.forw.inved)
user.forw.inved.no_na.no_init <- user.forw.inved.no_na[!(user.forw.inved.no_na$inv %in% c("srobledog@unal.edu.co", "martha.zuluaga@ucaldas.edu.co","tos_man@unal.edu.co")), ]
user.forw.inved.no_na.wom <- data.frame(table(user.forw.inved.no_na.no_init$id))
user.forw.inved.no_na.wom <- user.forw.inved.no_na.wom[!(user.forw.inved.no_na.wom$Freq==0),]
colnames(user.forw.inved.no_na.wom) <- c("id", "WOM_effect")
# user <- read.csv("user_4.csv")
user <- merge(user[,c(1:14,16:18)], user.forw.inved.no_na.wom, by = "id", # remove WOM_effect from user
              all.x = TRUE)
user$WOM_effect[is.na(user$WOM_effect)] <- 0
user <- user[,c(1:14,18,15:17)]

## add adoption

user_1 <- user_python
user_1$adoption <- ifelse(user_python$activation == is.na(user_python$activation), 0, 1)
user_1$adoption[is.na(user_1$adoption)] <- 0

## Add effective_invitations_send

# Extract out-degree from network_act

metrics <- data.frame(
  effe_inv_send = degree(network_act, mode="out")
)

metrics$id <- rownames(metrics)
metrics <- metrics[,c(2,1)]
row.names(metrics) <- NULL
user <- merge(user, metrics, all.x = TRUE)


## add invitations_send

# Extract out-degree from network

metrics <- data.frame(
  invitations_send = degree(network, mode="out")
)

metrics$id <- rownames(metrics)
metrics <- metrics[,c(2,1)] 

# Merge iser_1 and metrics

user_2 <- merge(user_1, metrics)

## Add sender_type variable in user_1

user_2$sender_type <- ""
user_2$sender_type <- ifelse(user_2$id %in% c("srobledog@unal.edu.co", "martha.zuluaga@ucaldas.edu.co", "tos_man@unal.edu.co"), "entrepreneur", 
                             ifelse(user_2$id %in% c("eamorab@unal.edu.co", "dmgallegoh@unal.edu.co", "dcherrerah@unal.edu.co", "ambenitezg@unal.edu.co", "smarredondol@gmail.com", "almarinfl@unal.edu.co", "jfrancoh@unal.edu.co"), "influencer",
                                    ifelse(user_2$id %in% c("javivaresv@unal.edu.co", "jczuluagag@unal.edu.co", "vtabaresm@gmail.com", "coparrap@unal.edu.co", "juarestrepogu@unal.edu.co", "miguel.solis@correounivalle.edu.co", "diheab@hotmail.com", "clopez@icesi.edu.co", "lcarolina@utp.edu.co", "mercedessuarez17@gmail.com", "diony.ico@correounivalle.edu.co", "maaperezvi@unal.edu.co", "paulina.toro@udea.edu.co", "vhborday@unal.edu.co"), "promoter", 
                                           ifelse(user_2$adoption == 1 & user_2$invitations_send == 0, "non_promoter", 
                                                  ifelse(user_2$invitations_send >= 0 & user_2$adoption == 1, "typic_user", "non_active")))))

## Adding network actors according with Fabian´s definition

user$net_actor <- ""
user$net_actor <- ifelse(user$adoption == 0, "not_active",
                         ifelse(user$invitations_send == 0, "not_forwarding",
                                ifelse(user$invitations_send > 0 & user$invitations_send < 257, "forwarding", "initiator")))


## Add Activation delay 

user$Activation_delay <- user$activation - user$invitation_date


## Add Invitations received (in  degree)

network_all <- graph.data.frame(invitation[,c(1,2)], directed = TRUE, vertices = user[,1] )
in_degree <- data.frame(
  id_1 <- user[,1],
  inv_received = degree(network_all, mode = "in")
)
colnames(in_degree) <- c("id", "inv_received") 
row.names(in_degree) <- NULL


user <- merge(user, in_degree) # cbind(user, in_degree[, "inv_received"] )
colnames(user_1)[12] <- "inv_received"

#### Extract several graphs:  ####
### Extract active users 

active_user <- user_2[user_2$adoption == 1,]

### Extract users who send 0 invitations and more than 0

active_user_inv_0 <- active_user[active_user$invitations_send == 0,]
active_user_great_inv_0 <- active_user[active_user$invitations_send > 0,]

### Extract type of users 

entrepreneurs <- active_user_great_inv_0[active_user_great_inv_0$sender_type == "entrepreneur",]
influencers <- active_user_great_inv_0[active_user_great_inv_0$sender_type == "influencer",]
promoters <- active_user_great_inv_0[active_user_great_inv_0$sender_type == "promoter",]
typic_user <- active_user_great_inv_0[active_user_great_inv_0$sender_type == "typic_user",]
non_promoters <- active_user[active_user$sender_type == "non_promoter",]

#### Visualization descriptive analysis ####

# Graph 1

graph_2 <- c(nrow(non_promoters), nrow(active_user_great_inv_0))
colors = c("blue", "red")
percentlabels <- round(100*graph_2/sum(graph_2),1)
pielables <- paste(percentlables, "%", sept= "")
pie(graph_2, labels= pielabels, main = "Non-promoteres vs users > 0 invitations", col = colors)
legend("topleft", c("Non-promoters", "users > 0 inv"), cex=0.8, fill= colors)

# Graph 2

graph_3 <- c(nrow(typic_user), nrow(promoters), nrow(influencers), nrow(entrepreneurs))
colors = c("red", "green", "blue", "yellow")
percentlabels <- round(100*graph_3/sum(graph_3),1)
pielabels <- paste(percentlabels, "%", sept= "")
pie(graph_3, labels= pielabels, main ="Network Actors", col = colors)
legend("topleft", c("ave_user", "promoters", "influencers", "entrepreneurs"), cex=0.8, fill=colors)

# Graph 3 - Network Actors 1

graph_3 <- c(nrow(users[users$invitations_send == 1,]), 
             nrow(users[users$invitations_send > 1 & !(users$sender_type_1 %in% "initiator"),]),
             nrow(users[users$sender_type_1 == "initiator",]))

colors = c("red", "purple", "yellow")
percentlabels <- round(100*graph_3/sum(graph_3),1)
pielabels <- paste(percentlabels, "%", sept= "")
pie(graph_3, labels= pielabels, main ="Network Actors", col = colors)
legend("topleft", c("send 1 invitation", "send more than 1 invitation", "inititators"), cex=0.8, fill=colors)

# Graph 4 - Delay activation analysis

hist(user$Activation_delay, 
     main="Histogram for delay time on activation",
     xlab = "Count of delay time on activation", 
     border = "blue",
     col= "green")

# Graph 5 - Timeline promotion

timeline <- data.frame(days=numeric(), active=numeric(), 
                       no_active=numeric(), stringsAsFactors = FALSE)

for (day in 1:367) {
  net_1 <- induced.subgraph(network, which(V(network)$invitation_date<=day))
  net_2 <- delete.edges(net_1, which(E(net_1)$invitation_date > day ))
  active <- induced.subgraph(net_2, which(V(net_2)$activation <= day))
  no_active <- induced.subgraph(net_2, which(V(net_2)$activation > day | is.na(V(net_2)$activation)))
  newrow <- data.frame(days = day, active = vcount(active), no_active = vcount(no_active))
  timeline <- rbind(timeline, newrow)
}

timeline_1 <- melt(timeline, id.vars = "days")
ggplot(timeline_1, aes(x=days, y = value, colour = variable)) + 
  geom_line() +
  ggtitle("Activations vs Non activations through time") +
  guides(fill=FALSE)

# Graph 6 - Timeline usage

usage_timeline <- data.frame(table(usage$activation_date))
days <- data.frame(days=c(1:367))
usage_timeline_days <- merge(days, usage_timeline,all.x = TRUE,
                             by.x = "days", by.y = "Var1")
usage_timeline_days[,2][is.na(usage_timeline_days[,2])] <- 0

ggplot(usage_timeline_days, aes(x = days, y = Freq, group=1)) +
  geom_line(colour = "blue") +
  xlab("Days (1:367)") +
  ylab("Number of files uploads") +
  ggtitle("Timeline usage")

# Graph 7 - WOM activations

days <- data.frame(day = c(1:367))

inv_day <- data.frame(table(invitation$invitation_date))
inv_day_1 <- merge(days, inv_day, by.x = "day", by.y = "Var1", all.x = TRUE)
inv_day_1[,2][is.na(inv_day_1[,2])] <- 0
colnames(inv_day_1) <- c("day", "inv_freq")

act_day <- data.frame(table(user$activation))
act_day_1 <- merge(days, act_day, by.x = "day", by.y = "Var1", all.x = TRUE)
act_day_1[,2][is.na(act_day_1[,2])] <- 0
colnames(act_day_1) <- c("day", "act_freq")

user_1 <- invitation[invitation$Source %in% c("srobledog@unal.edu.co", "martha.zuluaga@ucaldas.edu.co", "tos_man@unal.edu.co"), ]
dummy <- unique(user_1$Target)
user_2 <- user[!(user$id %in% dummy), ]
user_3 <- user_2[!(user_2$id %in% c("srobledog@unal.edu.co", 
                                    "martha.zuluaga@ucaldas.edu.co", "tos_man@unal.edu.co")),]
wom_day <- data.frame(table(user_3$activation))
wom_day_1 <- merge(days, wom_day, by.x = "day", by.y = "Var1", all.x = TRUE)
wom_day_1[,2][is.na(wom_day_1[,2])] <- 0
colnames(wom_day_1) <- c("day", "wom_freq")

inv_act_wom <- join_all(list(inv_day_1, act_day_1, wom_day_1), by="day")

inv_act_wom_1 <- melt(inv_act_wom, id="day")
ggplot(inv_act_wom_1,
       aes(x = day, y = value, colour=variable )) +
  geom_line() +
  xlab("Days (1:367)") +
  ylab("Frequency") +
  ggtitle("Invitations vs Activations vs WOM") +
  scale_color_manual(values = c( "green", "blue", "red"))



#### Locations ####

user_python$location <- str_extract(user_python$id, "(?<=\\@)[^.]+")
dummy_1 <- table(user_python$location)
lbls <- paste(names(dummy_1), "\n", dummy_1, sep = "")
pie(dummy_1, labels = lbls, 
    main = "Pie Chart of location\n (whith sample sizes)")

#### Usage Analysis ####

active <- user[user$adoption==1 & user$sender_type_1 != "initiator",c("adoption","usage")]
active_0 <- nrow(active[active$usage == 0,])
usage_1 <- nrow(active[active$usage > 0,])

pie_data <- c(active_0, usage_1)
colors = c("blue", "green")
percentlabels <- round(100*pie_data/sum(pie_data), 1)
pielabels <- paste(percentlabels, "%", sept="")
pie(pie_data, labels = pielabels, main = "Active with no usage vs Active with at least 1 usage", 
    col=colors)
legend("topleft", c("Active no usage", "Active usage"), cex=0.8, fill=colors)

#### Inferential Analysis - Hans proposal ####

user_1 <- user[!(user$id %in% c("srobledog@unal.edu.co", "martha.zuluaga@ucaldas.edu.co", "tos_man@unal.edu.co")),]
user_inf <- user_1[, c("adoption", "sender_type_1", "inv_received")]
user_inf$adoption <- as.integer(user_inf$adoption)
user_inf$sender_type_1 <- factor(user_inf$sender_type_1)
xtabs(~ adoption + inv_received, data = user_inf)

#### Network Analysis ####

d.network <- degree(network, mode="out")
hist(d.network, col="blue",
     xlab="Out-degree", ylab="Frequency",
     main="Out-Degree Distribution")

dd.network <- degree.distribution(network)
d <- 1:max(d.network)-1
ind <- (dd.network != 0)
plot(d[ind], dd.network[ind], log="xy", col="blue", 
     xlab=c("log-out-degree"), ylab=c("Log-Intensity"),
     main="Log-Log Out Degree Distribution")

d.network_act <- degree(network_act, mode="out")
hist(d.network_act, col="blue",
     xlab="Out-degree", ylab="Frequency",
     main="Out-Degree Distribution")

dd.network_act <- degree.distribution(network_act)
d <- 1:max(d.network_act)-1
ind <- (dd.network_act != 0)
plot(d[ind], dd.network_act[ind], log="xy", col="blue", 
     xlab=c("log-out-degree"), ylab=c("Log-Intensity"),
     main="Log-Log Out Degree Distribution")



#### WOM: Network Actors ####
#### Identifying network actors 

## Adding new variables...
# amount of days invitations

net_1 <- as_data_frame(network_act)
net_1 <- net_1[c(1,3)]
net_2 <- aggregate(invitation_date~., net_1, FUN=count)
net_2 <- data.frame(table(net_1$from ,  net_1$invitation_date))
net_2 <- count(net_1, c("from", "invitation_date"))

days_amount <- net_2[,c(1,2)]
days_amount <- data.frame(table(days_amount$from))
names(days_amount) <- c("id", "days_amount" )

user <- merge(user, days_amount, all.x = TRUE)
user_1[is.na(user_1$days_amount),] <- 0 # There is a mistake here...

# Amount of individual and grupal invitations

net_2$ind_inv <- ifelse(net_2$freq == 1, 1, 0)
net_2$group_inv <- ifelse(net_2$freq > 1, 1, 0)
ind_inv <- aggregate(net_2$ind_inv, by = list(id = net_2$from), FUN=sum)
names(ind_inv) <- c("id", "ind_inv")
group_inv <- aggregate(net_2$group_inv, by = list(id = net_2$from), FUN=sum)
names(group_inv) <- c("id", "group_inv")

user <- merge(user, ind_inv, all.x = TRUE)
user <- merge(user, group_inv, all.x = TRUE)

# Adding 0 values to NA 

user[is.na(user$days_amount), "days_amount" ] <- 0
user[is.na(user$ind_inv), "ind_inv" ] <- 0
user[is.na(user$group_inv), "group_inv" ] <- 0

### Segmentation data ####

user.forw <- user[user$net_actor == "forwarding", 
                  c("invitations_send", "effe_inv_send",
                    "usage","days_amount")]
# Group differences 



net.act_1 <- net.act[, c("usage", "effe_inv_send", "days_amount", "ind_inv", "group_inv")]
seg.net.act_1 <- user[!(user$sender_type == "entrepreneur" | user$sender_type == "non_promoter"),]
seg.net.act <- seg.net.act_1[,c("usage", "Activation_delay", "inv_received", "effe_inv_send", "days_amount", 
                                "ind_inv","group_inv")]

#### Clustering 

## function

seg.summ <- function(data, groups) {
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))
}

# Hierarchical Clustering: hclust() 

seg.dist <- daisy(net.act_1)
seg.hc <- hclust(seg.dist, method = "complete")
plot(seg.hc)
plot(cut(as.dendrogram(seg.hc), h=50)$lower[[1]])

## How good is the hierarchical clustering

cor(cophenetic(seg.hc), seg.dist)

## Hierarchical Clustering Continued: Groups from hclust()

plot(seg.hc)
rect.hclust(seg.hc, k=3, border="red")

seg.hc.segment <- cutree(seg.hc, k=3)     # membership vector for 3 groups
table(seg.hc.segment)

seg.summ(net.act_1, seg.hc.segment)  # Inspect clusters

##  Mean-BasedClustering:kmeans()

set.seed(96743)
seg.k <- kmeans(net.act_1, centers=3)

seg.summ(net.act_1, seg.k$cluster)
boxplot(net.act_1$effe_inv_send ~ seg.k$cluster, ylab="Inv_send", xlab="Cluster")

clusplot(net.act_1, seg.k$cluster, color=TRUE, shade=TRUE,
         labels=3, lines=0, main="K-means cluster plot")

## Model-BasedClustering:Mclust()

seg.mc <- Mclust(seg.net.act)
summary(seg.mc)

# With three clusters

seg.mc3 <- Mclust(seg.net.act, G=3)
summary(seg.mc3)

## ComparingModelswithBIC()

BIC(seg.mc, seg.mc4)

#### Disconnect with database ####
dbDisconnect(db)


#### Ch 2 An Overview of the R language ####
# A quick tour of R's Capabilities

# With all network actors

user_no_na <- user
user_no_na[is.na(user_no_na)] <- 0
any(is.na(user_no_na))  # Just  to check if we have NA values

summary(user)
corrplot.mixed(cor(user_no_na[,c(8:11,13:14)]))

aggregate(effe_inv_send ~ net_actor, user_no_na, mean)
aggregate(cbind(invitations_send, effe_inv_send, usage, inv_received, Activation_delay, WOM_effect ) ~ net_actor, 
          user_no_na, mean)

# Does effective invitations send differ by network actor, 
# but are the differences statistically significant?

effe_inv_send.anova <- aov(effe_inv_send ~ -1 + net_actor, user_no_na)
summary(effe_inv_send.anova)

par(mar=c(4,8,4,2))
plot(glht(effe_inv_send.anova))

# With forwarding actors

# Transforming and standardizing the data

autoTransform <- function(x) {
  library(forecast)
  return(scale(BoxCox(x, BoxCox.lambda(x))))
}

## We need to improve this code ##

user.forw.t <- user.forw.raw # d for dummy
user.forw.t$trans <- autoTransform(user.forw.t$effe_inv_send)

## Proposed network actors inside forwarding category ##

user.forw.t$forw_type <- ifelse(user.forw.t$trans <= 1, "low_forwarding",
                                ifelse(user.forw.t$trans > 1 & user.forw.t$trans <=  2 , 
                                       "strong_forwarding", "influencer"))

table(user.forw.t$forw_type)


# check http://www.r-bloggers.com/a-quick-primer-on-split-apply-combine-problems/

summary(user.forw.t)
corrplot.mixed(cor(user.forw.t[,c(8:11,13:15,19)]))

aggregate(effe_inv_send ~ forw_type, user.forw.t, mean)
aggregate(cbind(invitations_send, effe_inv_send, usage, inv_received, Activation_delay, WOM_effect ) ~ forw_type, 
          user.forw.t, mean)

# Does Does effective invitations send differ by network actor, 
# but are the differences statistically significant?
# We do need to transform data (normalize)
effe_inv_send.anova <- aov(effe_inv_send ~ -1 + forw_type, user.forw.t)
summary(effe_inv_send.anova)

par(mar=c(4,8,4,2))
plot(glht(effe_inv_send.anova))

# Structural Equation Model

WOMModel <- "NET_ACT =~ inv_received
WOM     =~ invitations_send + effe_inv_send
WOM ~ NET_ACT  "

WOM_MODEL.fit <- cfa(WOMModel, data = user)
summary(WOM_MODEL.fit, fit.m = TRUE)

semPaths(WOM_MODEL.fit, what="est",
         residuals = FALSE, intercepts = FALSE, nCharNodes = 9)

#### Ch 3 Describing Data ####

aggregate(cbind(influencer =user$influencer, strong_forw=user$strong_forw, strong_forw=user$low_forw)  ~ user$net_actor_forw, user, sum)

with(user.dt, table(net_actor_forw, influencer))

# QQ Plot to check normality

qqnorm(user$invitations_send)
qqline(user$invitations_send)

## Cumulative distribution

inv_cum <- data.frame(table(invitation$invitation_date))
days <- data.frame(Var1 = as.factor(c(1:366)), stringsAsFactors = TRUE)
inv_cum_total <- merge(inv_cum, days, all.y = TRUE)
inv_cum_total[is.na(inv_cum_total)] <- 0

plot(ecdf(inv_cum_total$Freq),  ## better interpretation...
     main="Cumulative Distribution of Daily Invitations",
     ylab="Cumulative Proportion",
     xlab="Daily invitations for 367 days",
     yaxt="n")
axis(side=2, at=seq(0,1,by=0.1), las=1,
     labels=paste(seq(0,100,by=10), "%", sep=""))
abline(h=0.9, lty=3)
abline(v=quantile(inv_cum_total$Freq, pr=0.9), lty=3)


#### Ch 4 Relationships Between Continuous Variables ####

pairs(user[,c(6:15)])
gpairs(user[,c(6:15)])
scatterplotMatrix(user[,c(6:15)])
cor(user$invitations_send, user$effe_inv_send, use = "complete")
cor.test(user$invitations_send, user$effe_inv_send, use = "complete")

corrplot.mixed(corr=cor(user[,c(7:15)], use = "complete.obs"),
               upper="ellipse", tl.pos="lt",
               col=colorpanel(50, "red", "gray60", "blue4"))

# Transforming variables before computing correlations, not work :-(

powerTransform(user[user$invitations_send > 0, "invitations_send"])
lambda <- coef(powerTransform(1/user[user$invitations_send > 0, "invitations_send"]))
bcPower(user[user$invitations_send > 0, "invitations_send"], lambda)

par(mfrow=c(1,2))
hist(user[user$invitations_send > 0, "invitations_send"],
     xlab="Invitations send", ylab="Count of Invitations",
     main="Original Distribuion")
hist(bcPower(user[user$invitations_send > 0, "invitations_send"], lambda),
     xlab="Box-Cox Transform of Invitations Send", ylab="Count of Invitations",
     main="Transformed Distribution")

#### Ch 5 Comparing Groups: Tables and visualizations #### 

by
aggregate(invitations_send ~ net_actor_forw, user, mean)
with(user, table(invitations_send, net_actor_forw))
xtabs(influencer ~ net_actor_forw, user)

aggregate(cbind(influencer, strong_forw, low_forw) ~ net_actor_forw, user , sum)

# Visualizing by group: frequencies and proportions

histogram(~usage | net_actor_forw, user)
histogram(~usage | net_actor_forw, user, type = "count", 
          layout=c(4,1), col=c("burlywood", "darkolivegreen"))

prop.table(table(user$usage, user$net_actor_forw), margin=2)

doubledecker(table(user[user$net_actor_forw %in% c("influencer", "strong_forw", "low_forw"),c(20:23)]))

# Visualizing by group: Continuous Data

seg.mean <- aggregate(invitations_send ~ net_actor_forw, user, mean)
barchart(invitations_send ~ net_actor_forw, seg.mean, col="grey")

boxplot(invitations_send ~ net_actor_forw, user, yaxt="n", ylab="Invitations send")

#### Comparing groups: Statistical Tests ####

#### Ch 9 Additional Linear Modelling Topics ####

invitations.m1 <- lm(invitations_send ~ ., 
                     data=user[user$invitations_send > 0, c(6:15)])
summary(invitations.m1)

autoTransform <- function(x) {
  library(forecast)
  return(scale(BoxCox(x, BoxCox.lambda(x))))
}

user.bc <- user[complete.cases(user), c(7:15)]
user.bc <- user.bc[user.bc$invitations_send > 0,]
numcols <- which(colnames(user.bc) != "email")
user.bc[, numcols] <- lapply(user.bc[,numcols], autoTransform)

summary(user.bc)
gpairs(user.bc)

invitations.m2 <- lm(invitations_send ~ ., 
                     data=user.bc)
summary(invitations.m2)

invitations.m3 <- lm(invitations_send ~ effe_inv_send, user.bc)
anova(invitations.m3, invitations.m2)

# Basic of the logistic Regression Model

aggregate(cbind(influencer, strong_forw, low_forw) ~ net_actor_forw, user , sum)

net_actor.m1 <- glm(net_actor_forw ~ influencer + strong_forw + low_forw, user, family = binomial)
summary(net_actor.m1)

# Multinomial Logistic Regression (Not in book)

user.mlr <- user[user$net_actor_forw %in% c("influencer", "strong_forwarding", "low_forwarding"),
                 c(20:23)]
user.mlr$net_actor_forw2 <- relevel(user.mlr$net_actor_forw, ref = "influencer")

test <- multinom(net_actor_forw2 ~ influencer + strong_forw + low_forw, user.mlr)
summary(test)

z <- summary(test)$coefficients/summary(test)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1))*2


#### Ch 11 Segmentation: clustering and Classification ####
## The challenge is to identify net-act from forwarding
# Organizing data 

gpairs(user[,6:15])

user.forw.raw <- user[user$net_actor == "forwarding", 
                      c("id","effe_inv_send")]

# Transforming and standardizing the data

autoTransform <- function(x) {
  library(forecast)
  return(scale(BoxCox(x, BoxCox.lambda(x))))
}

## We need to improve this code ##

user.forw.t <- user.forw.raw # d for dummy
user.forw.t$trans <- autoTransform(user.forw.t$effe_inv_send)

## Proposed network actors inside forwarding category ##

user.forw.t$forw_type <- ifelse(user.forw.t$trans <= 1, "low_forwarding",
                                ifelse(user.forw.t$trans > 1 & user.forw.t$trans <=  2 , 
                                       "strong_forwarding", "influencer"))

table(user.forw.t$forw_type)

## Exploratory analysis of the results ##

seg.summ <- function(data, groups) {
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))  
}

seg.summ(user.forw, user.forw.d$forw_type)

seg.summ(user[user$net_actor == "forwarding", c("effe_inv_send", "usage", "days_amount")], 
         user.forw.t$forw_type)

user.forw <- user[user$net_actor == "forwarding",
                  c("effe_inv_send", "usage", "days_amount")]

boxplot(user.forw$effe_inv_send ~ user.forw.t$forw_type,
        main="Effective_inv vs Network Actor")

boxplot(user.forw$usage ~ user.forw.t$forw_type,
        main="Usage vs Network Actor")

boxplot(user.forw$days_amount ~ user.forw.t$forw_type,
        main="days amount vs Network Actor")


## Hierarchical Clustering ##

user.forw.dist <- daisy(user.forw)
as.matrix(user.forw.dist)[1:5,1:5]

user.forw.hc <- hclust(user.forw.dist, method = "complete")
plot(user.forw.hc)
plot(cut(as.dendrogram(user.forw.hc), h=21)$lower[[4]])
user.forw[c(1816,3612),] # It is not working...because the sequence

## cophenetic correlation coefficient ##

cor(cophenetic(user.forw.hc), user.forw.dist)

## Hierarchical Clustering Continued: Groups from hclust() ##

plot(user.forw.hc)
rect.hclust(user.forw.hc, k=3, border = "red")

## We obtain the assignment vector for observations ##

user.forw.hc.segment <- cutree(user.forw.hc, k=3)
table(user.forw.hc.segment)

## We inspect the variables: ##

seg.summ(user.forw, user.forw.hc.segment)

## Mean-Based Clustering: kmeans() ##

set.seed(96743)
user.forw.k <- kmeans(user.forw, centers = 3)

seg.summ(user.forw, user.forw.k$cluster)
boxplot(user.forw$effe_inv_send ~ user.forw.k$cluster,
        ylab="Effective_inv", xlab="Cluster",
        main="Effective_inv vs Cluster")
boxplot(user.forw$usage ~ user.forw.k$cluster,
        ylab="Usage", xlab="Cluster", 
        main="Usage vs Cluster")
boxplot(user.forw$days_amount ~ user.forw.k$cluster,
        ylab="Days Amount", xlab="Cluster", 
        main="Days Amount vs Cluster")

## dimensional reduction with principal components ##

clusplot(user.forw, user.forw.k$cluster, color=TRUE, shape=TRUE,
         lables=3, lines=0, main="K-means cluster plot")

# Model - Based Clustering: Mclust()

library(mclust)
user.forw.mc <- Mclust(user.forw)
summary(user.forw.mc)

seg.summ(user.forw, user.forw.mc$class)
clusplot(user.forw, user.forw.mc$class , color = TRUE, shade = TRUE,
         labels=4, lines = 0, main = "Model-Based cluster plot")

# Latent Class Analysis: poLCA() #
# Only categorical variables 

user.forw.cut <- user.forw
user.forw.cut$effe_inv_send <- factor(ifelse(user.forw$effe_inv_send < median(user.forw$effe_inv_send), 1, 2))
user.forw.cut$usage <- factor(ifelse(user.forw$usage < median(user.forw$usage), 1, 2))
user.forw.cut$days_amount <- factor(ifelse(user.forw$days_amount < median(user.forw$days_amount), 1, 2))

summary(user.forw.cut)

user.forw.f <- with(user.forw.cut, 
                    cbind(effe_inv_send, usage, days_amount)~1)

set.seed(02807)
user.forw.LCA4 <- poLCA(user.forw.f, data=user.forw.cut, nclass=4)
user.forw.LCA3 <- poLCA(user.forw.f, data=user.forw.cut, nclass=3)
user.forw.LCA2 <- poLCA(user.forw.f, data=user.forw.cut, nclass=2)

user.forw.LCA2$bic
user.forw.LCA3$bic
user.forw.LCA4$bic

seg.summ(user.forw, user.forw.LCA2$predclass)
table(user.forw.LCA2$predclass)
clusplot(user.forw, user.forw.LCA2$predclass, color=TRUE, shade=TRUE,
         labels = 4, lines=0, main="LCA plot (k=2)")

seg.summ(user.forw, user.forw.LCA3$predclass)
table(user.forw.LCA3$predclass)

clusplot(user.forw, user.forw.LCA3$predclass, color=TRUE, shade=TRUE,
         labels = 4, lines=0, main="LCA plot (k=3)")

# Comparing Clusters Solutions

### Network actors

adopters <- user[user$adoption==1,] #3029
not_forwarding <- adopters[adopters$effe_inv_send == 0,] # 2398
forwarding <- adopters[adopters$effe_inv_send > 0,] # 631
one_forwarding <- forwarding[forwarding$effe_inv_send== 1,] # 308

table(strong_forwarding$days_amount)

# Graph to understand strong_forwardings

dummy <- invitation[invitation$Source == "eamorab@unal.edu.co", ]
dummy_1 <- merge(dummy, user, by.x = "Target", by.y = "id", all.x = TRUE)
dummy_2 <- dummy_1$activation

plot(table(dummy$invitation_date))

## Another way 

aggregate(effe_inv_send ~ net.act, user, mean)
aggregate(usage ~ sender_type_1, user, mean)


usage.anova <- aov(usage ~ -1 + sender_type_1, user)
effe_inv_send.anova <- aov(effe_inv_send ~ -1 + sender_type_1, user)
summary(effe_inv_send.anova)

library(multcomp)
par(mar=c(4,8,4,2))
plot(glht(effe_inv_send.anova))

#### Ch 11 Segmentation clustering and classification WOM effect ####

gpairs(user[,c(9,15)])

user.forw.raw <- user[user$net_actor == "forwarding", 
                      c("id","effe_inv_send", "WOM_effect")]

# Transforming and standardizing the data

autoTransform <- function(x) {
  library(forecast)
  return(scale(BoxCox(x, BoxCox.lambda(x))))
}

## We need to improve this code ##

user.forw.raw <- user[user$net_actor == "forwarding",]
user.forw <- user.forw.raw[,c(7:15)]

user.forw.t <- user.forw.raw # d for dummy
user.forw.t$trans <- autoTransform(user.forw.t$effe_inv_send)


user.forw.t$forw_type <- ifelse(user.forw.t$trans <= 1, "low_forwarding",
                                ifelse(user.forw.t$trans > 1 & user.forw.t$trans <=  2 , 
                                       "strong_forwarding", "influencer"))

table(user.forw.t$forw_type)
table(user.forw.t[user.forw.t$forw_type == "low_forwarding", c( "effe_inv_send")])
table(user.forw.t[user.forw.t$forw_type == "strong_forwarding", c( "effe_inv_send")])
table(user.forw.t[user.forw.t$forw_type == "influencer", c( "effe_inv_send")])

hist(user.forw.t[user.forw.t$forw_type == "influencer", c( "effe_inv_send")], main = "Hist. Influencer vs effe_inv_send", xlab = "")
hist(user.forw.t[user.forw.t$forw_type == "strong_forwarding", c( "effe_inv_send")], main = "Hist. Influencer vs strong_forwarding", xlab = "")
hist(user.forw.t[user.forw.t$forw_type == "low_forwarding", c( "effe_inv_send")], main = "Hist. Influencer vs low_forwarding", xlab = "")

table(user$inv_received)

## Exploratory analysis of the results ##

seg.summ <- function(data, groups) {
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))  
}

seg.summ(user.forw, user.forw.d$forw_type)

boxplot(user.forw$effe_inv_send ~ user.forw.d$forw_type,
        main="Effective_inv vs Network Actor")

boxplot(user.forw$WOM_effect ~ user.forw.d$forw_type,
        main="WOM effect vs Network Actor")

boxplot(user.forw$days_amount ~ user.forw.d$forw_type,
        main="days amount vs Network Actor")


## Hierarchical Clustering ##

user.forw.dist <- daisy(user.forw)
as.matrix(user.forw.dist)[1:5,1:5]

user.forw.hc <- hclust(user.forw.dist, method = "complete")
plot(user.forw.hc)
plot(cut(as.dendrogram(user.forw.hc), h=21)$lower[[4]])
user.forw[c(1816,3612),] # It is not working...because the sequence

## cophenetic correlation coefficient ##

cor(cophenetic(user.forw.hc), user.forw.dist)

## Hierarchical Clustering Continued: Groups from hclust() ##

plot(user.forw.hc)
rect.hclust(user.forw.hc, k=3, border = "red")

## We obtain the assignment vector for observations ##

user.forw.hc.segment <- cutree(user.forw.hc, k=3)
table(user.forw.hc.segment)

## We inspect the variables: ##

seg.summ(user.forw, user.forw.hc.segment)

## Mean-Based Clustering: kmeans() ##

set.seed(96743)
user.forw.k <- kmeans(user.forw, centers = 3)

seg.summ(user.forw, user.forw.k$cluster)
boxplot(user.forw$effe_inv_send ~ user.forw.k$cluster,
        ylab="Effective_inv", xlab="Cluster",
        main="Effective_inv vs Cluster")
boxplot(user.forw$usage ~ user.forw.k$cluster,
        ylab="Usage", xlab="Cluster", 
        main="Usage vs Cluster")
boxplot(user.forw$days_amount ~ user.forw.k$cluster,
        ylab="Days Amount", xlab="Cluster", 
        main="Days Amount vs Cluster")

## dimensional reduction with principal components ##

clusplot(user.forw, user.forw.k$cluster, color=TRUE, shape=TRUE,
         lables=3, lines=0, main="K-means cluster plot")

# Model - Based Clustering: Mclust()

library(mclust)
user.forw.mc <- Mclust(user.forw)
summary(user.forw.mc)

seg.summ(user.forw, user.forw.mc$class)
clusplot(user.forw, user.forw.mc$class , color = TRUE, shade = TRUE,
         labels=4, lines = 0, main = "Model-Based cluster plot")

# Latent Class Analysis: poLCA() #
# Only categorical variables 

user.forw.cut <- user.forw
user.forw.cut$effe_inv_send <- factor(ifelse(user.forw$effe_inv_send < median(user.forw$effe_inv_send), 1, 2))
user.forw.cut$usage <- factor(ifelse(user.forw$usage < median(user.forw$usage), 1, 2))
user.forw.cut$days_amount <- factor(ifelse(user.forw$days_amount < median(user.forw$days_amount), 1, 2))

summary(user.forw.cut)

user.forw.f <- with(user.forw.cut, 
                    cbind(effe_inv_send, usage, days_amount)~1)

set.seed(02807)
user.forw.LCA4 <- poLCA(user.forw.f, data=user.forw.cut, nclass=4)
user.forw.LCA3 <- poLCA(user.forw.f, data=user.forw.cut, nclass=3)
user.forw.LCA2 <- poLCA(user.forw.f, data=user.forw.cut, nclass=2)

user.forw.LCA2$bic
user.forw.LCA3$bic
user.forw.LCA4$bic

seg.summ(user.forw, user.forw.LCA2$predclass)
table(user.forw.LCA2$predclass)
clusplot(user.forw, user.forw.LCA2$predclass, color=TRUE, shade=TRUE,
         labels = 4, lines=0, main="LCA plot (k=2)")

seg.summ(user.forw, user.forw.LCA3$predclass)
table(user.forw.LCA3$predclass)

clusplot(user.forw, user.forw.LCA3$predclass, color=TRUE, shade=TRUE,
         labels = 4, lines=0, main="LCA plot (k=3)")

# Comparing Clusters Solutions

### Network actors

adopters <- user[user$adoption==1,] #3029
not_forwarding <- adopters[adopters$effe_inv_send == 0,] # 2398
forwarding <- adopters[adopters$effe_inv_send > 0,] # 631
one_forwarding <- forwarding[forwarding$effe_inv_send== 1,] # 308

table(strong_forwarding$days_amount)

# Graph to understand strong_forwardings

dummy <- invitation[invitation$Source == "eamorab@unal.edu.co", ]
dummy_1 <- merge(dummy, user, by.x = "Target", by.y = "id", all.x = TRUE)
dummy_2 <- dummy_1$activation

plot(table(dummy$invitation_date))




