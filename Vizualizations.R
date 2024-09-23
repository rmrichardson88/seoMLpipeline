setwd("C:\\Users\\devro\\OneDrive\\Documents\\Syracuse\\IST718\\FinalProj\\")

#install.packages("jsonlite")
#install.packages("ggcorrplot")
#install.packages("wesanderson")

library(jsonlite)
library(tidyverse)
library(ggplot2)
library(ggcorrplot)
library(wesanderson)
library(dplyr)
library(plotly)

set.seed(1)




#########################################################################################################
### SEO Data ###
#########################################################################################################

df_seo <- read.csv2("seo_data.csv", header = TRUE, sep = ",")
str(df_seo)


# attempt to join cluster & distance back to the original cluster data.
# first join assigned and unassigned together.

json_assigned <- fromJSON("assigned_clusters.json")
df_assigned <- bind_rows(json_assigned)
str(df_assigned)

json_unassigned <- fromJSON("unassigned_queries_data.json")
df_unassigned <- as.data.frame(json_unassigned)
str(df_unassigned)

df_clustersAll <- bind_rows(df_assigned, df_unassigned)
str(df_clustersAll)

# join cluster data to seo data
df_merged <- merge(df_seo, df_clustersAll, by.x='Keyword', by.y='query')
df_merged$opportunity <- df_merged$Traffic.potential - df_merged$Volume
str(df_merged)
#view(df_merged)

# now look at things like volume and traffic potential by cluster
# separate clusters from keywords & SERP.features
df_metrics <- subset(df_merged, select=-c(Keyword,SERP.Features))
str(df_metrics)

df_volume_sum <- df_metrics %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), list(mean=mean, sd=sd), na.rm=TRUE))

str(df_volume_sum)
#view(df_volume_sum)

# need the cluster frequency to join back to the summary data
df_cfreq <- as.data.frame(table(df_metrics$cluster))
names(df_cfreq) <- c("cluster","freq_cluster")
head(df_cfreq)

# join back to summary data
df_volume_sum <- merge(df_volume_sum, df_cfreq)
df_volume_sum$opportunity <- df_volume_sum$Traffic.potential_mean - df_volume_sum$Volume_mean
df_volume_sum$cPalette <- topo.colors(n = nrow(df_volume_sum))
str(df_volume_sum)

# find medians of columns
var_medianMeanVol <- median(df_volume_sum$Volume_mean, 2, na.rm = TRUE)
#var_medianMeanVol
var_medianMeanGV <- median(df_volume_sum$Global.volume_mean, 2, na.rm = TRUE)
#var_medianMeanGV
var_medianMeanTP <- median(df_volume_sum$Traffic.potential_mean, 2, na.rm = TRUE)
#var_medianMeanTP
var_medianMeanDis <- median(df_volume_sum$distance_mean, 2, na.rm = TRUE)
#var_medianMeanDis
var_medianCFreq <- median(df_volume_sum$freq_cluster, 2, na.rm = TRUE)
#var_medianCFreq
var_medianOpp <- median(df_volume_sum$opportunity_mean, 2, na.rm = TRUE)
#var_medianOpp

# find top and bottom 10 clusters by frequency
df_topFreqClusters <- head(df_volume_sum[order(-df_volume_sum$freq_cluster),],10)
df_botFreqClusters <- tail(df_volume_sum[order(-df_volume_sum$freq_cluster),],10)
df_FreqClusters <- bind_rows(df_topFreqClusters,df_botFreqClusters)


# ggplot(df_FreqClusters, aes(x=cluster, y=freq_cluster)) +
#   geom_segment( aes(x=cluster, xend=cluster, y=var_medianCFreq, yend=freq_cluster), color="skyblue") +
#   geom_point( color="blue", size=4, alpha=0.6) +
#   theme_light() +
#   coord_flip() +
#   theme(
#     panel.grid.major.y = element_blank(),
#     panel.border = element_blank(),
#     axis.ticks.y = element_blank()
#   )

ggplot(df_FreqClusters, aes(x=cluster, y=freq_cluster)) +
  geom_segment(aes(x=cluster, xend=cluster, y=var_medianCFreq, yend=freq_cluster, colour = cPalette)) +
  geom_point(size=4, alpha=0.6, aes(colour = cPalette)) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )

# plot distribution of clusters with freq < top10

# find top and bottom 10 clusters by traffic potential
df_topTrafClusters <- head(df_volume_sum[order(-df_volume_sum$Traffic.potential_mean),],10)
df_botTrafClusters <- tail(df_volume_sum[order(-df_volume_sum$Traffic.potential_mean),],10)
df_TrafClusters <- bind_rows(df_topTrafClusters, df_botTrafClusters)

# ggplot(df_TrafClusters, aes(x=cluster, y=Traffic.potential_mean)) +
#   geom_segment( aes(x=cluster, xend=cluster, y=var_medianMeanTP, yend=Traffic.potential_mean), color="skyblue") +
#   geom_point( color="blue", size=4, alpha=0.6) +
#   theme_light() +
#   coord_flip() +
#   theme(
#     panel.grid.major.y = element_blank(),
#     panel.border = element_blank(),
#     axis.ticks.y = element_blank()
#   )

ggplot(df_TrafClusters, aes(x=cluster, y=Traffic.potential_mean)) +
  geom_segment( aes(x=cluster, xend=cluster, y=var_medianMeanTP, yend=Traffic.potential_mean, colour = cPalette)) +
  geom_point( size=4, alpha=0.6, aes(colour = cPalette)) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )

# find top and bottom 10 clusters by global volume
df_topGVClusters <- head(df_volume_sum[order(-df_volume_sum$Global.volume_mean),],10)
df_botGVClusters <- tail(df_volume_sum[order(-df_volume_sum$Global.volume_mean),],10)
df_GVClusters <- bind_rows(df_topGVClusters, df_botGVClusters)

ggplot(df_GVClusters, aes(x=cluster, y=Global.volume_mean)) +
  geom_segment( aes(x=cluster, xend=cluster, y=var_medianMeanGV, yend=Global.volume_mean), color="skyblue") +
  geom_point( color="blue", size=4, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )


# find top and bottom 10 clusters by volume
df_topVolClusters <- head(df_volume_sum[order(-df_volume_sum$Volume_mean),],10)
df_botVolClusters <- tail(df_volume_sum[order(-df_volume_sum$Volume_mean),],10)
df_VolClusters <- bind_rows(df_topVolClusters, df_botVolClusters)

ggplot(df_VolClusters, aes(x=cluster, y=Volume_mean)) +
  geom_segment( aes(x=cluster, xend=cluster, y=var_medianMeanVol, yend=Volume_mean), color="skyblue") +
  geom_point( color="blue", size=4, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )



# 3D plots

plot_ly(data=df_volume_sum, x=~Global.volume_mean, y=~Traffic.potential_mean, 
        z=~freq_cluster) %>% layout(scene = list(xaxis = list(title = 'AVG Glb Vol')
                                    , yaxis = list(title = 'Avg Tfc Pot')
                                    , zaxis = list(title = 'Cluster Freq')))


plot_ly(data=df_volume_sum, x=~Volume_mean, y=~Traffic.potential_mean, 
        z=~freq_cluster) %>% layout(scene = list(xaxis = list(title = 'AVG Vol')
                                                 , yaxis = list(title = 'Avg Tfc Pot')
                                                 , zaxis = list(title = 'Cluster Freq')))



df_assignedOpps <- df_volume_sum[which(df_volume_sum$distance_mean < 1.412),]
df_top10Opps <- head(df_assignedOpps[order(-df_assignedOpps$opportunity_mean),],10)
#view(df_top10Opps)

ggplot(data=df_top10Opps, aes(x=cluster, y=opportunity_mean, fill=cluster))+
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


df_unassignedOpp <- df_volume_sum[which(df_volume_sum$distance_mean > 1.412),]
df_top10UAOpps <- head(df_unassignedOpp[order(-df_unassignedOpp$opportunity_mean),],10)
#view(df_top10UAOpps)

ggplot(data=df_top10UAOpps, aes(x=cluster, y=opportunity_mean, fill=cluster))+
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))



# violin plot greatest opportunity
#A2A637
topOpp <- df_merged[which(df_merged$cluster == 'Crop Video'),]
#topOpp
ggplot(topOpp, aes(x=Volume, y=Traffic.potential))+
  geom_violin(fill = "#A2A637")

#1CBDC2
topUAOpp <- df_merged[which(df_merged$cluster == 'Object Remover'),]
topUAOpp
ggplot(data=subset(topUAOpp, !is.na(Traffic.potential)), aes(x=Volume, y=log(Traffic.potential))) +
  geom_violin(fill = "#1CBDC2")








