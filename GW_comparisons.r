rm(list=ls())
library(dplyr)
library(ggplot2)

#######################################################
## Novartis look and feel functions
#######################################################
Novartis.Color.Palette <- structure(
  c(  "#0460A9", "#CDDFEE", "#9BBFDD", "#68A0CB", "#03487F", "#023054",
      "#E74A21", "#FADBD3", "#F5B7A6", "#F1927A", "#AD3819", "#742510",
      "#EC9A1E", "#FBEBD2", "#F7D7A5", "#F4C278", "#B17416", "#764D0F",
      "#8D1F1B", "#E8D2D1", "#D1A5A4", "#BB7976", "#6A1714", "#46100E",
      "#7F7F7F", "#E5E5E5", "#CCCCCC", "#B2B2B2", "#5F5F5F", "#404040",
      "#CCCCCC", "#F5F5F5", "#EBEBEB", "#E0E0E0", "#999999", "#666666",
      "#404040", "#D9D9D9", "#B3B3B3", "#8C8C8C", "#303030", "#202020"
  ),.Dim = c(6L, 7L),
  .Dimnames = list(c("Hue", "Tint3", "Tint2", "Tint1", "Shade1", "Shade2"),
                   c("Novartis Blue", "Sienna", "Apricot", "Carmine", 
                     "Gray", "Light Gray", "Dark Gray")))

ggColor <- function(n.colors = 50, color.values = c("Hue", "Tint1", "Shade1"), 
                    color.hues = c(1:4)){
  color.vec <- as.vector(t(Novartis.Color.Palette[color.values,color.hues]))
  
  rep.colors <- 1
  if(n.colors > length(color.vec)){
    rep.colors <- ceiling(n.colors/length(color.vec))
  }
  
  ggcolor <- scale_color_manual(values = rep(color.vec,rep.colors))
  return(ggcolor)
}

ggFill <- function(n.colors = 42, color.values = c("Hue", "Tint1", "Shade1"), 
                   color.hues = c(1:4)){
  color.vec <- as.vector(t(Novartis.Color.Palette[color.values,color.hues]))
  
  rep.colors <- 1
  if(n.colors > length(color.vec)){
    rep.colors <- ceiling(n.colors/length(color.vec))
  }
  
  ggcolor <- scale_fill_manual(values = rep(color.vec,rep.colors))
  return(ggcolor)
}



##################################################################################
## Facilitating Comparisons - Proximity & Alignment
## Demostration of direct labels on a simple line graph
## Direct labels reduce burden to decode
##################################################################################



## make data
df <- data.frame(trt=rep(c("A", "B", "C"), each=4),
                 visit=rep(c(1, 2, 3, 4),3),
                 response=c(4.9,3.2,2.1,2.5, 
                            3.4,3.8,1.3,0.8,
                            4.1,3.6,2.1,1.5)
)

## plot with legend look up
ggplot(df, aes(x=visit, y=response, group=trt, colour=trt)) + 
  geom_line(size=1.35, aes(colour=trt)) + 
  scale_x_continuous(limits = c(0.5, 4.5), breaks = c(1,2,3,4)) + 
  ggColor() +
  theme_minimal(base_size = 10 ) +  
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        legend.title=element_blank(),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        legend.text=element_text(size = 20),
        legend.justification=c(0,0), legend.position=c(0,0)
  )   

ggsave(file="Plot15a.png", width = 80, height = 80, units = "mm", dpi = 300)


## plot with direct label
ggplot(df, aes(x=visit, y=response, group=trt)) + 
  geom_line(size=1.35, aes(colour=trt)) + 
  scale_x_continuous(limits = c(0.5, 4.5), breaks = c(1,2,3,4)) +
  ggColor()+ 
  geom_text(data = df[df$visit == "4",], 
            aes(label = trt), size = 8, hjust = -0.5, vjust = 0.25) +
  theme_minimal(base_size = 10 ) +  
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        legend.title=element_blank(),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        legend.position="none"
  ) 

ggsave(file="Plot15b.png", width = 80, height = 80, units = "mm", dpi = 300)



###################################################################################
## Demostration of a panelled and overlayed density plot
## Proximity 
## Keep quantities to be compared close together. 
###################################################################################

## Make data
set.seed(12345666)
n <- 100
overlay <- data.frame(v=c("1","2"), mu=c(1,1.2))
overlay <- overlay[rep(c(1,2),n),]
overlay$s <- rnorm(n=2*n,mean=overlay$mu, sd=1)


## Set theme
theme_set(theme_minimal(base_size=18))
th <- theme(panel.grid.minor=element_blank(),
            panel.grid.major=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            strip.background = element_rect(fill = "lightgrey", colour = "grey50"),
            panel.background = element_rect(fill = "white", colour = "grey50")
)


## Plot by panel
ggplot(overlay, aes(x=s)) + 
  facet_wrap(~v, ncol=1) + 
  geom_line(stat="density",size=1) + 
  th

ggsave(file="Plot16a.png", width = 80, height = 80, units = "mm", dpi = 300)


## Plot by overlay
ggplot(overlay, aes(x=s, colour=factor(v))) + 
  geom_line(stat="density", size=1) + 
  ggColor() +  th +
  theme(legend.position = "none") + 
  guides(color = guide_legend(title=NULL))+
  geom_text(data=data.frame(x=c(1,2), y=c(0.4, 0.3), v = c("1", "2")),
            aes(x= x, y= y, label = factor(v), color = factor(v)),hjust = -0.1, vjust = 0.25, size = 8)

ggsave(file="Plot16b.png", width = 80, height = 80, units = "mm", dpi = 300)



####################################################################################
## Ordering of a simple dot plot
## Ordering values is helpful for comparing across many categories.
####################################################################################

set.seed(12345666)
theme_set(theme_minimal(base_size=18))
ordering <- data.frame(v=LETTERS[1:7], y=runif(7))

ggplot(ordering, aes(y=y, x=v)) + 
  geom_point(size=5)  +
  coord_flip() + 
  theme(panel.grid.major.y = element_line(colour = "grey60", size = 0.8),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.y=element_blank()
  )

ggsave(file="Plot17a.png", width = 80, height = 80, units = "mm", dpi = 300)


## order plot to display a ranking 
ordering <- arrange(ordering, y)
ggplot(ordering, aes(y=y, x=factor(v, levels=ordering$v))) + 
  geom_point(size=5) + 
  coord_flip() + 
  theme(panel.grid.major.y = element_line(colour = "grey60", size = 0.8),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.y=element_blank()
        #axis.text.y=element_blank()
  )

ggsave(file="Plot17b.png", width = 80, height = 80, units = "mm", dpi = 300)


########################################################################################
## Simple forest plot 
## Oriernation can simplify comparisons
########################################################################################
## Set common theme
theme_set(theme_minimal(base_size=18))
th <- theme(panel.grid.minor.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.background = element_rect(fill = "white", colour = "grey50"),
            axis.title.y=element_blank(),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank())

## Make data
align <- data.frame(x=factor(c(1,2,3)),y=c(2,1,3),low=c(1,0,2),hi=c(3,2,4))

ggplot(align, aes(x=x, y=y, ymin=low, ymax=hi)) + 
  geom_point(size=5) + 
  geom_linerange(size=1) + 
  th

ggsave(file="Plot18a.png", width = 80, height = 80, units = "mm", dpi = 300)

## flip the orientation to support a simpler comparison
last_plot() + coord_flip()

ggsave(file="Plot18b.png", width = 80, height = 80, units = "mm", dpi = 300)


#########################################################################################
## Simple forest plot
## Differences - do the calculation 
## Avoid the reviewer making the calculation; 
## show the difference where applicable 
## e.g. plot the mean difference rather individual means.
#########################################################################################

## Set theme
theme_set(theme_minimal(base_size=18))
th <- theme(panel.grid.minor=element_blank(),
            panel.grid.major=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            strip.background = element_rect(fill = "lightgrey", colour = "grey50"),
            panel.background = element_rect(fill = "white", colour = "grey50")
)


## Make data
difference <- data.frame(grp=c("PBO", "TRT1", "TRT2"), 
                         y=c(1,2,2.2), 
                         low=c(0,1,1.1), 
                         hi=c(2,3,3.1))

ggplot(difference, aes(x=grp, y=y, ymin=low, ymax=hi)) + 
  geom_pointrange(size=1) + 
  scale_y_continuous(breaks=c(0,1,2,3)) + 
  coord_flip() + 
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.y=element_blank(),
        plot.background = element_rect(fill = "white", colour = "grey50")
  )

ggsave(file="Plot19a.png", width = 80, height = 80, units = "mm", dpi = 300)


difference2 <- data.frame(grp=c("TRT1\n-PBO","TRT2\n-PBO"), 
                          y=c(1,1.2), 
                          low=c(-0.3,-0.1), 
                          hi=c(2.3,2.5))

ggplot(difference2, aes(x=grp, y=y, ymin=low, ymax=hi)) + 
  geom_hline(yintercept=0, linetype=2) + 
  geom_pointrange(size=1) + 
  scale_y_continuous(breaks=c(0,1,2,3)) + 
  coord_flip() +  
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.y=element_blank(),
        plot.background = element_rect(fill = "white", colour = "grey50")
  )

ggsave(file="Plot19b.png", width = 80, height = 80, units = "mm", dpi = 300)


####################################################################################
## Simple scatter plot
## Layering and visual anchoring
## Draw attention to the comparison by utilising grid lines, 
## reference lines and other visual anchors to add layer and context. 
####################################################################################


set.seed(12345666)

# make data
grl <- data.frame(x=c(0,1,2,4,8,12,24,48,72,84), y=rnorm(200))

## Set theme
theme_set(theme_minimal(base_size=18))
th <- theme(panel.grid.minor=element_blank(),
            panel.grid.major=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            strip.background = element_rect(fill = "lightgrey", colour = "grey50"),
            panel.background = element_rect(fill = "white", colour = "grey50")
)



## plot
ggplot(grl, aes(x=x, y=y)) + 
  geom_point(size=5) +  
  th

ggsave(file="Plot20a.png", width = 80, height = 80, units = "mm", dpi = 300)


last_plot() + 
  geom_hline(yintercept=0, linetype=1, color="red", size=4) +
  theme(panel.grid.minor=element_line(color = "lightgrey", size = 0.5),
        panel.grid.major=element_line(color = "lightgrey", size = 0.5)
  )
ggsave(file="Plot20b.png", width = 80, height = 80, units = "mm", dpi = 300)

