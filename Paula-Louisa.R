setwd("C:/Users/Louisa/Dropbox/Overseas/Copenhagen/Courses/Social Network Analysis/Project")

#----------------------------------------------#
##### ----- *** LOADING PACKAGES *** ----- #####
#----------------------------------------------#

### loading packages
library(dplyr)
library(igraph)
library(ggplot2)  
library(ForceAtlas2)

# --------------------#
#   Load graph file   #
# --------------------#

## reading a graphml file in your working directory as a graph object
g <- read.graph ("15-comments.graphml", format = "graphml")

## inspect g to find out what it contains and in this case, which attribute to use
g


#--------------------------------------#
##### ----- *** INDEGREE *** ----- #####
#--------------------------------------#

## convert g into a new data frame with indegree calculation 
df.indegree <- data.frame(Channel = V(g)$label, # using the channel IDs attribute in the first column 
                          indegree_norm = degree(g, 
                                                 mode = "in", 
                                                 normalized = T), # normalized indegree
                          indegree = degree(g, 
                                            mode = "in",
                                            normalized = F )) # raw indegree
                           

## inspect df.indegree object just for confirmation
df.indegree

## arrange by indegree, highest to lowest with all entries and re-assigning it to object df.indegree
df.indegree <- df.indegree %>% arrange(-indegree_norm)

## examining the first 30 rows (top 30 channels ranked by indegree)
head(df.indegree, 30)

## export as .csv file 
csv_indegree <- head(df.indegree, 30)
write.csv(csv_indegree, file = "csv_indegree.csv")

# measuring graph degree centralisation - 0.2144645
centralization.degree(g)


#-----------------------------------------#
##### ----- *** BETWEENNESS *** ----- #####
#-----------------------------------------#

## calculating betweenness, converning g into a data.frame with directed entries
df.betweenness <- data.frame(Channel = V(g)$label, 
                             betweenness_norm = betweenness(g, 
                                                       directed = TRUE, 
                                                       weights = NULL, 
                                                       nobigint = FALSE, 
                                                       normalized = TRUE),
                             betweenness = betweenness(g, 
                                                       directed = TRUE, 
                                                       weights = NULL, 
                                                       nobigint = FALSE, 
                                                       normalized = FALSE))

## arrange betweenness data in order
df.betweenness <- arrange(df.betweenness, -betweenness_norm)

## examining the first 30 rows (top 30 channels ranked by betweenness) 
head(df.betweenness, 30)

## export as .csv file 
csv_betweenness <- head(df.betweenness, 30)
write.csv(csv_betweenness, file = "csv_betweenness.csv")

# measuring graph degree centralisation - 0.03345247
centralization.betweenness(g)

#-----------------------------------------#
##### ------ *** CLOSENESS *** ------ #####
#-----------------------------------------#

### unused in the end because graph is disconnected ###
## Warning Message:  In closeness(g, mode = c("in"), weights = NULL, normalized = TRUE) : 
## At centrality.c:2617 :closeness centrality is not well-defined for disconnected graphs  

## calculating closeness, converning g into a data.frame with directed entries
df.closeness <- data.frame(Channel = V(g)$label, 
                             closeness_norm = closeness(g, 
                                                        mode = c("in"), 
                                                        weights = NULL, 
                                                        normalized = TRUE),
                             closeness = closeness(g, 
                                                  mode = c("in"), 
                                                  weights = NULL, 
                                                  normalized = FALSE))
                     
                           
## arrange closeness data in order
df.closeness <- arrange(df.closeness, -closeness_norm)

## examining the first 30 rows (top 30 channels ranked by closeness) 
head(df.closeness, 30)

## export as .csv file 
csv_closeness <- head(df.closeness, 30)
write.csv(csv_closeness, file = "csv_closeness.csv")


#-------------------------------------------------#
##### ----- *** ADD COLUMN *** ----- #####
#-------------------------------------------------#

## change to data frame
gdf <- as.data.frame(get.edgelist(g))

## add indegree scores as new column 
gdf$newcolumn <- df.indegree
                                   
View(g)

#-------------------------------------------------#
##### ----- *** COMMUNITIES, GRAPHS *** ----- #####
#-------------------------------------------------#

## making it undirected
g.undirected <- as.undirected(g)
g.undirected <- simplify(g.undirected, remove.loops = T)

## Using MODULARITY ## 
modularity <- cluster_louvain(g.undirected)
names(modularity) # find out what names there are in modularity

plot.igraph(g.undirected,
            layout = layout.fruchterman.reingold,
            main = "YouTube Anti-Vaccine Network",
            edge.arrow.size = .5,
            vertex.size = 2,
            vertex.label = NA,
            vertex.label.color = "black",
            vertex.label.cex = 1,
            vertex.label.font.size = 0.1,
            vertex.frame.color = NA,
            vertex.color = modularity$membership)

write_graph(g.undirected, file = "ggraph.graphml")
