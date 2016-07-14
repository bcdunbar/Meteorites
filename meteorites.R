##
#
# Author: Branden Dunbar
# 
# 
# Exploring meteorites dataset in r with ggplot and plotly
#

library(ggplot2)
library(plotly)
library(maptools)



###############################################
#         read in meteorites data             #
###############################################
meteorites <- read.csv("Meteorite_Landings_new.csv")
View(meteorites)
head(meteorites)

##############################################
#        stats (class, type, group)          #
##############################################

############ Class ###############
## find the most freq class
w <- table(meteorites$class)
t <- as.data.frame(w)
t$rank <- rank(-t$Freq,ties.method="max")
class_freq <- t[order(t$rank,decreasing = F),]
## subset of ordered data
class_subset <- subset(class_freq[1:10, ])

## plot class_freq
plot_classfreq <-  ggplot(class_freq, aes(x=rank,y=Freq, label= Var1)) +
    geom_point() +
    geom_label(colour = "white", fontface = "bold", fill = "blue") +
    ggtitle("Meteorite Impacts (by class)") +
    labs(x="Rank",y="Freq")

plot_classfreq

## alternate way to visualise class freq with geom_bar
ggplot(data = meteorites) + 
  geom_bar(mapping = aes(x = class, fill = class), width = 1)


############ Type ###############
## find the most freq type
ww <- table(meteorites$type)
tt <- as.data.frame(ww)
tt$rank <- rank(-tt$Freq,ties.method="max")
type_freq <- tt[order(tt$rank,decreasing = F),]
## subset of ordered data
type_subset <- subset(type_freq[1:10, ])

## plot type_freq
plot_typefreq <-  ggplot(type_freq, aes(x=rank,y=Freq, label= Var1, size = Freq)) +
  geom_point() +
  geom_label(colour = "white", fontface = "bold", fill = "blue") +
  ggtitle("Meteorite Impacts (by type)") +
  labs(x="Rank",y="Freq")

plot_typefreq

## alternate way to visualise type freq with geom_bar
ggplot(data = meteorites) + 
  geom_bar(mapping = aes(x = type, fill = type), width = 1)


############ Group ###############
## find the most freq group
www <- table(meteorites$group)
ttt <- as.data.frame(www)
ttt$rank <- rank(-ttt$Freq,ties.method="max")
group_freq <- ttt[order(ttt$rank,decreasing = F),]
## subset of ordered data
group_subset <- subset(group_freq[1:10, ])

## plot group_freq
plot_groupfreq <-  ggplot(group_subset, aes(x=rank,y=Freq, label= Var1, size = Freq)) +
  geom_point() +
  geom_label(colour = "white", fontface = "bold", fill = "blue") +
  ggtitle("Meteorite Impacts (by class)") +
  labs(x="Rank",y="Freq")

plot_groupfreq

## alternate way to visualise group freq with


###########################################
#         interactive world map           #
###########################################

## ggplot + plotly
## interactive map (meteorite landings by class)
viz <- ggplot(meteorites, aes(reclong, reclat)) +
  borders("world", colour="gray50", fill="gray50") +
  coord_equal() +
  geom_point(aes(text = name, colour = class), alpha = .5) +
  ggtitle("Meteorites (by class)") +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank()) 

# viz - view static map

vis_plotly <- ggplotly(viz)

## publish to plotly account
# Sys.setenv("plotly_username"="bdun9")
# Sys.setenv("plotly_api_key"= "ukqr128tmk")
# plotly_POST(vis_plotly)


## interactive map (meteorite landings by type)
viz_type <- ggplot(meteorites, aes(reclong, reclat)) +
  borders("world", colour="gray50", fill="gray50") +
  coord_equal() +
  geom_point(aes(text = name, colour = type), alpha = .5) +
  ggtitle("Meteorites (by type)") +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank()) 


#################################################
#  create shiny server/ui for interactive plot  #
#################################################

# check server/ui files in folder for shiny application

