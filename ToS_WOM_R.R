library(igraph)
library(stringr)
library(sqldf)
library(ggplot2)
library(reshape2)
library("plyr", lib.loc="~/R/win-library/3.3")

#### Connect to database ####
db <- dbConnect(SQLite(), dbname="EM_NM.sqlite")
user <- dbReadTable(db, "user")
invitation <- dbReadTable(db, "invitation")
usage <- dbReadTable(db, "usage")

# Another alternative... database sometimes do not have data!
user <- read.csv("users.csv")
invitation <- read.csv("invitations.csv")
usage <- read.csv("usage.csv")

# With this information create the networks
network <- graph.data.frame(invitation, directed = TRUE, user)
network_act <- induced_subgraph(network, V(network)[adoption == 1])

#### Getting data Read files and create the network ####

user_python <- read.csv("user.csv")
user_python$X <- NULL
user_python$invitation_date <- as.numeric(user_python$invitation_date)
user_python$activation <- as.numeric(user_python$activation)
invitation_python <- read.csv("invitation.csv")
invitation_python$id <- NULL
invitation_python$invitation_date <- as.numeric(invitation_python$invitation_date)

network <- graph.data.frame(invitation_python, directed = TRUE, user_python )

#### Add new variables to user_1 ####
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
user <- merge(user, metrics)


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
                            
## Add Activation delay 

user$Activation_delay <- user$activation - user$invitation_date


## Add Invitations received (in  degree)

network <- graph.data.frame(invitation[,c(1,2)], directed = TRUE, vertices = user[,1] )
out_degree <- data.frame(
  id_1 <- user[,1],
  inv_received = degree(network, mode = "in")
)

user_1 <- cbind(user, out_degree[, "inv_received"] )
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
## Identifying network actors 

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

user_1 <- merge(user, days_amount, all.x = TRUE)
user_1[is.na(user_1$days_amount),] <- 0 # There is a mistake here...

# Amount of individual and grupal invitations

net_2$ind_inv <- ifelse(net_2$freq == 1, 1, 0)
net_2$group_inv <- ifelse(net_2$freq > 1, 1, 0)
ind_inv <- aggregate(net_2$ind_inv, by = list(id = net_2$from), FUN=sum)
names(ind_inv) <- c("id", "ind_inv")
group_inv <- aggregate(net_2$group_inv, by = list(id = net_2$from), FUN=sum)
names(group_inv) <- c("id", "group_inv")

user_2 <- merge(user_1, ind_inv, all.x = TRUE)
user_3 <- merge(user_2, group_inv, all.x = TRUE)

#### Disconnect with database ####
dbDisconnect(db)



