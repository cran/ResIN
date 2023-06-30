## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
# install.packages("ResIN")

library(ggplot2)
library(ResIN)


## ----simulate data, message=FALSE, warning=FALSE------------------------------

data(lik_data)


## ----run ResIN, message=FALSE, warning=FALSE----------------------------------
# Apply the ResIN function to toy Likert data:

ResIN_output <- ResIN(lik_data, cor_method = "spearman", network_stats = TRUE)


## ----plotting nodes, message=FALSE, warning=FALSE, fig.align='center', fig.width=12, fig.height=9----

ggplot(ResIN_output$node_frame, aes(x = x, y = y))+
  geom_point(size = 0.25, color = "grey")+
  geom_smooth(method = "lm", se = FALSE, linetype = 2, color = "red")+
  geom_label(data = ResIN_output$node_frame, aes(x = x, y = y, label = node_names, color = Strength), size = 5)+
  ylim(c(-4, 4))+
  theme_bw()+
  ggtitle("Item-Response Node Latent Positions")

 

## ----plotting edges, message=FALSE, warning=FALSE, fig.align='center', fig.width=12, fig.height=9----

edge_plot <- ggplot() +
    geom_curve(data = ResIN_output$edgelist_frame, aes(x = from.x, xend = to.x, y = from.y,
                yend = to.y, size = weight^5), curvature = 0.15, color = "darkgrey", alpha = 0.8) +
    geom_label(data = ResIN_output$edgelist_frame, aes(x = from.x, y = from.y, label = from, color = Strength), size = 5) +
    geom_label(data = ResIN_output$edgelist_frame, aes(x = to.x, y = to.y, label = to, color = Strength), size = 5) +
                  ## Make sure to plot node labels both based on the to and from columns in the plotting frame
    ggtitle("ResIN Network in 2D Latent Space")+
    labs(color = "Node strength centrality: ")+
    ylim(c(-4, 4))+
    scale_size_continuous(guide = "none")+
    theme_bw()+
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          axis.text.y = element_blank(), axis.title.y = element_blank(),
          axis.ticks = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), legend.position = "bottom",
          legend.text = element_blank(), plot.title = element_text(hjust = 0.5, size=18))


edge_plot


## ---- warning=FALSE, message=FALSE, mysize=TRUE, size='\\scriptsize'----------

## Integration with qgraph package
resin_qgraph <- ResIN_qgraph(lik_data, plot_graph = TRUE,
                    qgraph_arglist = list(layout = "spring", maximum = 1, vsize = 4,
                    DoNotPlot = TRUE, sampleSize = nrow(lik_data),
                    title = "ResIN plot made with qgraph package",
                    mar = c(1,1,1,1), normalize = FALSE))


## ---- warning=FALSE, message=FALSE, mysize=TRUE, size='\\scriptsize'----------

## Integration with igraph package
resin_igraph <- ResIN_igraph(lik_data)

set.seed(12)
igraph::plot.igraph(resin_igraph$igraph_obj)


