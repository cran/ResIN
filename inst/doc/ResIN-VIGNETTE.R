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
 set.seed(42)
 n <- 1000
 k <- 8

 latent_dgp <- rnorm(1000, 0, 1)

 ## Data for k manifest indicators measured with some error
 cont_data <- matrix(rep(latent_dgp, k), n, k)
 cont_data <- apply(cont_data, 2, function(x) {
   x + rnorm(1000, 0, 0.2)}) ## 20% error

 ## Ordinal, 5-point Likert-scale items
 lik_maker <- function(x) {
   sort(runif(x, min = -2, max = 2))}

 lik_data <- matrix(NA, n, k)

 for(i in 1:ncol(lik_data)) {
   lik_data[, i] <- findInterval(cont_data[, i], vec = c(-Inf, lik_maker(4), Inf))
 }



## ----run ResIN, message=FALSE, warning=FALSE----------------------------------
 # Apply the ResIN function to toy Likert data:

 output <- ResIN(lik_data, cor_method = "spearman", network_stats = TRUE)


## ----plotting, message=FALSE, warning=FALSE, fig.align='center', fig.width=12, fig.height=9----

# Create a basic outcome plot with ggplot
 output$ggplot_frame <- output$ggplot_frame[order(output$ggplot_frame$Strength,
                                                  decreasing = FALSE), ]
 ResIN_plot <- ggplot2::ggplot(output$ggplot_frame) +
   geom_curve(data = output$ggplot_frame, aes(x = from.x, xend = to.x, y = from.y,
                                              yend = to.y, linewidth = weight,
                                              color = Strength), curvature = 0.2) +
   geom_point(aes(x = from.x, y = from.y, shape = as.factor(cluster)), size = 8)+
   geom_point(aes(x = to.x, y = to.y), size = 8)+
   geom_text(data = output$ggplot_frame, aes(x = from.x, y = from.y, label = from),
             size = 3, color = "white") +
   geom_text(data = output$ggplot_frame, aes(x = to.x, y = to.y, label = to),
             size = 3, color = "white") +
   ggtitle("ResIN example  plot")+
   theme_dark()+
   theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
         axis.text.y = element_blank(), axis.title.y = element_blank(),
         axis.ticks = element_blank(), panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(), legend.position = "none",
         legend.text = element_blank(), plot.title = element_text(hjust = 0.5))

 ResIN_plot
 

