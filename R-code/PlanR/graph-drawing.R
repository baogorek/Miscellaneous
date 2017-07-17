library(igraph)  
library(Cairo) 

nodes <- read.csv("myNodes.csv", header = T, stringsAsFactors = FALSE)  
edges <-read.csv("myEdges.csv", header = T, stringsAsFactors = FALSE)
                 
nodes$storyInd = nodes$name %in% edges$from
nodes$taskInd = nodes$name %in% edges$to

toDoList <- read.csv("ToDoList.csv", header = T, stringsAsFactors = FALSE)
names(toDoList) <- c("to", "from")

toDoItems <- toDoList$to
nItems = length(toDoItems)

edges <- rbind(edges, toDoList)
nodes <- rbind(nodes, data.frame(name = toDoItems, storyInd = rep(F, nItems),
                                 taskInd = rep(F, nItems) ) )

g <- graph.data.frame(edges, directed = T, vertices = nodes)
g$layout = layout.fruchterman.reingold

V(g)$label <- V(g)$name

V(g)[nodes$storyInd ==T]$color <- rgb(0,0,1,.2)
V(g)[nodes$taskInd ==T]$color <- rgb(0,1,0,.4)
V(g)[nodes$storyInd == F & nodes$taskInd == F]$color <- rgb(0,.5, .5,.3)

V(g)[nodes$storyInd ==T]$size <- 40
V(g)[nodes$taskInd ==T]$size <- 10
V(g)[nodes$storyInd == F & nodes$taskInd == F]$size <-5

names(igraph:::.igraph.shapes) 
V(g)[nodes$storyInd ==T]$shape <- 'rectangle'
V(g)[nodes$taskInd ==T]$shape <- 'circle'
V(g)[nodes$storyInd == F & nodes$taskInd == F]$shape <- 'circle'

V(g)[nodes$storyInd ==T]$label.font<- 2
V(g)[nodes$taskInd ==T]$label.font<- 3
V(g)[nodes$storyInd == F & nodes$taskInd == F]$label.font<-1

V(g)[nodes$storyInd ==T]$label.color<- "black"
V(g)[nodes$taskInd ==T]$label.color<- "blue"
V(g)[nodes$storyInd == F & nodes$taskInd == F]$label.color<-"red"


plot(g, ylim = c(1,-1), vertex.label.cex=1.2,
     vertex.frame.color=rgb(.5,.2,.3,.3))

getCoords <- function(){
   m <<- tkplot.getcoords(tk.g)
   print(m)
   tkplot.close(tk.g)
   CairoPNG(file = "ToDoGraph.png")
   plot(g, layout = m, ylim = c(1,-1), vertex.label.cex=1.2, vertex.frame.color=rgb(0,0,1,.01))
   dev.off()
   tkdestroy(tt)
   save(list = c("m", "g", "nodes", "edges"), file = "initialGraph.RData")
}


### MAIN  ###

tk.g <- tkplot(g, canvas.width = 600, canvas.height = 500)

tt <- tktoplevel()
label.widget <- tklabel(tt, text = "Push to Capture Configuration")
button.widget <- tkbutton(tt, text="Push",command = getCoords)
tkpack(label.widget, button.widget) # geometry manager

#####END MAIN#####

# to plot it here
g$layout <- m
plot(g, layout = m, ylim=c(1,-1))

## Add to-do list ##
g2 <- add.vertices(g,1)
V(g2)$name[8] = "8"
V(g2)$label[8] = "8"

new.edges = data.frame(from = c(0), to = c(7) )

g2 <- add.edges(g2, as.matrix(new.edges))
g2$layout = rbind(g2$layout, c(445, 400))

plot(g2, ylim = c(1,-1))

#Competing Method

g.components <- decompose.graph(g)
n.components <- length(g.components)

par(mfrow = c(n.components,1))
for(i in c(1:n.components)) {  
    plot( g.components[[i]], ylim=c(1,-1) , main = i,
          vertex.color="white", 
          vertex.size = 20, edge.arrow.size = .5, edge.color = "blue",
          layout = layout.reingold.tilford) 
}
