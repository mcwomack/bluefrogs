#load packages
library(phytools)
library(tcltk)
library(tidyverse)
library(geiger)
library(ggplot2)


#set working directory
setwd("/Users/mollywomack/Documents/Documents\ -\ Molly’s\ MacBook\ Pro/Pubs/InPrep/BlueFrogs")

##### FIGURE 2 - SOURCE COMPARISONS #####
allblue <-read.csv("Data/AxanthicRecords_Upload.csv", header=T)

allblue$Individuals.Observed[is.na(allblue$Individuals.Observed)] = 0 #assume that if number of individuals observed is NA then they refer to one individual

bluesource<- allblue %>% 
  group_by(Source) %>%
  summarise(individuals = sum(Individuals.Observed), cases = n()) 

bluesource<- bluesource %>% 
  mutate(across('Source', str_replace, 'iNat', 'iNaturalist'))

#rename and reorder columns
bluesource$Source <- factor(bluesource$Source, levels=c('iNaturalist','Sci Lit','News or Social')) #reorder x axis categories

p<-ggplot(data=bluesource, aes(x=Source, y=individuals)) +
  geom_bar(stat="identity", color="black", fill = c("lightblue", "steelblue1","blue")) + 
  labs(y = "Number of reported axanthic indviduals") +
  theme_classic()
p

pdf(file = paste("SourceCompare",".pdf",sep="_"), width = 5,  height = 5)
p
dev.off()


inatdata <- allblue %>% filter(Source == "iNat")
newsdata <- allblue %>% filter(Source == "News or Social")
litdata <- allblue %>% filter(Source == "Sci Lit")
bluedata <- allblue %>% filter(Source != "iNat")

sum(inatdata$Individuals.Observed)
unique(inatdata$Family)
unique(inatdata$Genera)
unique(inatdata$Species)
unique(inatdata$Country)

sum(inatdata$Individuals.Observed)
unique(inatdata$Family)
unique(inatdata$Genera)
unique(inatdata$Species)
unique(inatdata$Country)

##### FIGURE 3 - BLUE MAP #####
#load data
allblue$Longitude <- as.numeric(allblue$Longitude)
allblue$Latitude <- as.numeric(allblue$Latitude)
inatdata <- allblue %>% filter(Source == "iNat")


gg1 <- ggplot() + 
  geom_polygon(data=map_data("world"), aes(x=long, y=lat, group=group),
               color ="grey", fill= "white") + 
  geom_point(data=inatdata, aes(y = Longitude, x = Latitude, group=Source), size = 2, color='blue', alpha=0.5) + 
  #geom_jitter(width = 100, height = 100, color='blue', size=2, alpha=0.25) +
  coord_equal() +
  xlab('Longitude') + 
  ylab('Latitude') + 
  #  stat_density2d(aes(x = longitude, y = latitude), alpha = .5,
  #                geom = "polygon", data = bluedata) + 
  #scale_fill_manual(values = c("blue", "lightblue")) + 
  theme(panel.background = element_rect(fill = 'grey', color = 'grey'),
        panel.grid.major = element_line(color = 'grey', linetype = 'dotted'),
        panel.grid.minor = element_line(color = 'grey', size = 2)) #legend.position = 'none'

pdf(file = paste("BlueMap_INat",".pdf",sep="_"), width = 10,  height = 8)
gg1
dev.off()



bluedata <- allblue %>% filter(Source != "iNat")

gg2<-ggplot() + 
  geom_polygon(data=map_data("world"), aes(x=long, y=lat, group=group),
               color ="grey", fill= "white") + 
  geom_point(data=bluedata, aes(x = Longitude, y = Latitude, color=Source), size = 2, alpha=1) + 
  coord_equal() +
  xlab('Longitude') + 
  ylab('Latitude') + 
  scale_color_manual(values=c("lightblue", "steelblue1")) +
  theme(panel.background = element_rect(fill = 'grey', color = 'grey'),
        panel.grid.major = element_line(color = 'grey', linetype = 'dotted'),
        panel.grid.minor = element_line(color = 'grey', size = 2), 
        legend.position = 'none')

pdf(file = paste("BlueMap",".pdf",sep="_"), width = 10,  height = 8)
gg2
dev.off()

##### FIGURE 4 - FAMILY TREE FIGURE #####
#load family tree
FamilyTree<-read.tree("HimeFamilyTree2020_4FamiliesAdded.tre")
FamilyTree<-ladderize(FamilyTree, right = FALSE)

#load data
bluefam<-read.csv("Data/Axanthic_FamilyData.csv", header=T)
bluefam<-bluefam %>% drop_na(AmphibiaWeb_Sp)

#trim frog tree of life to species in your dataset
rownames(bluefam)<-bluefam$Family
name.check(FamilyTree,bluefam)->overlap
overlap$data_not_tree
drop.tip(FamilyTree,overlap$tree_not_data)->FamilyTree;
#plot(ecotree)
#Remove excess data from species not found in tree
bluefam<-bluefam[!rownames(bluefam) %in% overlap$data_not_tree,]
bluefam[] <- lapply(bluefam, function(x) if(is.factor(x)) factor(x) else x)
bluefam<-bluefam[FamilyTree$tip.label, ]

bluefam$tip<-paste(bluefam$Family," (", bluefam$AmphibiaWeb_Sp, "/", bluefam$iNatSp_checked, ")", sep = "")
rownames(bluefam)<-bluefam$Family
bluefam<-bluefam[FamilyTree$tip.label,]
FamilyTree$tip.label <- bluefam$tip
rownames(bluefam)<-bluefam$tip

#Color Tips
colr<-rep("white", length(bluefam[,1])) # set up a vector
colr[bluefam$BlueFrogs_YN =="Y"]<-"blue"
colr[bluefam$BlueFrogs_YN =="N"]<-"grey90"
names(colr)<-bluefam$tip # name the colour vector with the name of each species
colr<-colr[FamilyTree$tip.label] # sort the colours so they match the same order as the tip labels in the tree

#Plot tree
plot(FamilyTree, show.tip.label= T, font=3, cex=.5, label.offset = 1, edge.width=3)
tiplabels(pch=15, col=colr, cex=1.5)

#segments(70, 1:nrow(bluefam), 70 + (bluefam$AmphibiaWeb_Sp*0.02), 1:nrow(bluefam), col="grey", lwd=5)
#segments(70, 1:nrow(bluefam), 70 + (bluefam$iNatSp_checked*0.02), 1:nrow(bluefam), col="lightblue", lwd=5)

pdf(file = paste("FamilyTree_Blue",".pdf",sep="_"), width = 8,  height = 8)
plot(FamilyTree, show.tip.label= T, font=3, cex=.5, label.offset = 1, edge.width=3)
tiplabels(pch=15, col=colr, cex=1.5)
dev.off()







