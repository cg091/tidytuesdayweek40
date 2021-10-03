library(readr)
library(ggplot2)
library(dplyr)
library(scaler)

papers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/papers.csv')
authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/authors.csv')
programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/programs.csv')
paper_authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_authors.csv')
paper_programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_programs.csv')

#Showing table of frequencies of 'nr. of authors' for papers in Finance program_category
#Apply the same logic for the other two categories
gigi_Finance<-paper_authors%>%
  left_join(paper_programs)%>%
  left_join(programs)%>%
  filter(program_category=="Finance")

gigi_Finance<-gigi_Finance |> 
  group_by(paper,author) |> 
  summarise(n = n()) |> 
  count(paper)

plot<-ggplot(gigi_Finance, aes(x=n, fill=..count..))+ 
  scale_fill_gradient(low="ivory3", high="navy")+
  geom_bar() +
  labs(title="Nr. of co-authors in Finance (NBER dataset 1973-2021)")+
  geom_text(stat='count', aes(label=after_stat(count)), vjust=-1)+
  scale_x_continuous(name="Nr. of authors", limits = c(0,20), breaks_width(2, offset=2))+
  scale_y_continuous(name="Total nr. of papers") 

plot + theme(
  plot.title = element_text(color="black", size=16, face="bold.italic"),
  axis.title.x = element_text(color="gray10", size=11, face="bold"),
  axis.title.y = element_text(color="firebrick3", size=11, face="bold")
)


#Network Analysis for the Finance group of papers
coautori_finance<- paper_authors%>%
  left_join(authors)%>%
  left_join(paper_programs)%>%
  left_join(programs)%>%
  filter(program_category=="Finance")
coautori_finance<-coautori_finance[c("paper", "name", "program_category")]

coautori_finance<-coautori_finance |> 
  group_by(paper,name) |> 
  summarise(n = n()) |> 
  count(paper)

coautori_finance<-coautori_finance %>%
  group_by(paper) %>%
  summarise(name = paste(name, collapse = ", "))

library(data.table)

coautori_finance$count <- sapply(strsplit(coautori_finance$name,','), uniqueN)

df.coautori_finance = sapply(as.character(coautori_finance$name), strsplit, ", ")
df.coautori_finance= lapply(df.coautori_finance, trimws)
df.coautori_finance.unique = unique(unlist(df.coautori_finance))[order(unique(unlist(df.coautori_finance)))]

df.coautori_finance.bipartite.edges = lapply(df.coautori_finance, function(x) {df.coautori_finance.unique %in% x})
df.coautori_finance.bipartite.edges = do.call("cbind", df.coautori_finance.bipartite.edges) 
rownames(df.coautori_finance.bipartite.edges) = df.coautori_finance.unique

df.mat = df.coautori_finance.bipartite.edges %*% t(df.coautori_finance.bipartite.edges)  

library(igraph)

#Converting into a igraph object

gs<-graph_from_adjacency_matrix(df.mat)

#We are going to contracting the network by means of cluresting
set.seed(242)
cl<-walktrap.community(gs, steps=5)
cl$degree<- (degree(gs)[cl$names])
cl$cluster<-unname(ave(cl$degree,cl$membership,
                             FUN=function(x)names(x)[which.max(x)])
)

V(gs)$name <- cl$cluster

E(gs)$weight <- 1
V(gs)$weight <- 1
gcon <- contract.vertices(gs, cl$membership, 
                          vertex.attr.comb = list(weight = "sum", name = function(x)x[1], "ignore"))
gcon <- simplify(gcon, edge.attr.comb = list(weight = "sum", function(x)length(x)))

gcc <- induced.subgraph(gcon, V(gcon)$weight > 20)
V(gcc)$degree <- unname(degree(gcc))

set.seed(242)
par(mar = rep(0.1, 4)) 
g.layout <- layout.kamada.kawai(gcc)
plot.igraph(gcc, edge.arrow.size = 0.1, layout = g.layout, vertex.size = 0.5 * (V(gcc)$degree))

