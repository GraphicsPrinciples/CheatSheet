# Color for emphasis or distinction

rm(list=ls())
library(ggplot2)
library(dplyr)
library(reshape2)
library(gridExtra)

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


##############################################################################
##  Simple bar chart
##############################################################################

# Do not use color to differentiate between categories of the same variable. 
# The second version of the graph below is more effective. 
# The reader is much more likely to compare the bars when they look alike than 
# when they look different.

## Make data
df <- data.frame(trt=c("A", "B", "C","D","E"),
                 cause=c(4,6,10,12,15),
                 highlight = c(2,2,2,1,2))

a1 <- ggplot(df, aes(x=trt, y=cause, group=trt, fill=trt)) +
  geom_bar(width=0.5, stat = "identity") +  
  geom_hline(yintercept = 0, colour = "wheat4", linetype=1, size=0.8)+ 
  ggFill() +
  scale_y_continuous(breaks=c(0, 5, 10)) +
  coord_flip() +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(), 
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        legend.position="none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

ggsave(a1, file="GWCOL_a1.png", width = 80, height = 80, units = "mm", dpi = 300)

a2 <- ggplot(df, aes(x=trt, y=cause, group=trt, fill=trt)) +
  geom_bar(width=0.5, stat = "identity") +  
  geom_hline(yintercept = 0, colour = "wheat4", linetype=1, size=0.8)+ 
  ggFill(color.values = "Hue", color.hues = 1) +
  scale_y_continuous(breaks=c(0, 5, 10)) + 
  coord_flip() +
  theme(panel.grid.minor=element_blank(), 
        panel.grid.major.y=element_blank(),
        axis.ticks = element_blank(), 
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        legend.position="none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

ggsave(a2, file="GWCOL_a2.png", width = 80, height = 80, units = "mm", dpi = 300)


#########################################################################
##  Simple waterfall plot
#########################################################################

## Distinction
## Use colors or shades to represent meaningful differences.

## generate data
set.seed(123)
dat <- data.frame(x=1:10, ratio=sort(runif(10,0,2)))

#create flag
dat$col_flag <- dat$ratio > 1

b1 <- ggplot(dat, aes(color=factor(ratio))) +
  geom_segment(aes(x=x,xend=x,y=1, yend=ratio), size=8) +
  geom_hline(yintercept = 1, colour = "wheat4", linetype=1, size=1)+ 
  theme_minimal(base_size=9) + ggColor() + 
  theme(panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank(),
        axis.ticks = element_blank(), 
        axis.text = element_blank(),
        legend.position="none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank()
  )

ggsave(b1, file="GWCOL_b1.png", width = 80, height = 80, units = "mm", dpi = 300)


b2 <- ggplot(dat, aes(color=col_flag)) +
  geom_segment(aes(x=x,xend=x,y=1, yend=ratio), size=8) +
  geom_hline(yintercept = 1, colour = "wheat4", linetype=1, size=1)+ 
  theme_minimal(base_size=9) + ggColor(color.hues=c(2,1)) + 
  theme(panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank(),
        axis.ticks = element_blank(), 
        axis.text = element_blank(),
        legend.position="none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

ggsave(b2, file="GWCOL_b2.png", width = 80, height = 80, units = "mm", dpi = 300)


###################################################################
## Simple horizontal bar chart
###################################################################

## Consistency
## Be consistent. Use the same colors to  mean the same things in a series of graphs.


## make data
distance <- data.frame(expand.grid(study=c("Study 1","Study 2"), trt=c("A","B")))
distance$y <- c(1.2, 1.1, 1.8, 1.9)

c1 <- ggplot(distance, aes(x=trt, y=y, ymin=0, ymax=y,  colour=factor(paste0(study,"-",trt)))) + 
  facet_wrap(~study, ncol=2) + geom_linerange(size=10) + 
  th +  ggColor() +
  theme(axis.text.y = element_blank())+
  theme(legend.position = "top") +  
  theme(axis.text.y = element_blank()) + 
  guides(color = "none") 

ggsave(c1, file="GWCOL_c1.png", width = 80, height = 80, units = "mm", dpi = 300)


# Panels removed, groups of interest put next to one another.

c2 <- ggplot(distance, aes(x=trt, y=y, ymin=0, ymax=y,  colour=factor(trt)))+ 
  facet_wrap(~study, ncol=2) + geom_linerange(size=10) + 
  th +  ggColor() +
  theme(axis.text.y = element_blank())+
  theme(legend.position = "top") +  
  theme(axis.text.y = element_blank()) + 
  guides(color = "none") 

ggsave(c2, file="GWCOL_c2.png", width = 80, height = 80, units = "mm", dpi = 300)


#################################################################
##  Simple bar chart
#################################################################

## Using color for emphasis
## Use a darker shade or different colour to highlight/emphasize a focal point.

# Make data
df <- data.frame(trt=c("A", "B", "C","D","E"),
                 cause=c(4,6,10,12,15),
                 highlight = c(2,2,2,1,2))

d1 <- ggplot(df, aes(x=trt, y=cause, group=trt, fill=trt)) +
  geom_bar(width=0.5, stat = "identity") +  
  geom_hline(yintercept = 0, colour = "wheat4", linetype=1, size=0.8)+ 
  ggFill(color.values = "Hue", color.hues = 1) +
  scale_y_continuous(breaks=c(0, 5, 10)) + 
  coord_flip() +
  theme(panel.grid.minor=element_blank(), 
        panel.grid.major.y=element_blank(),
        axis.ticks = element_blank(), 
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        legend.position="none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

ggsave(d1, file="GWCOL_d1.png", width = 80, height = 80, units = "mm", dpi = 300)


d2 <- ggplot(df, aes(x=trt, y=cause, group=trt, fill=factor(highlight))) + 
  geom_bar(width=0.5, stat = "identity") +  
  geom_hline(yintercept = 0, colour = "wheat4", linetype=1, size=0.8)+ 
  ggFill(color.values = c("Shade2","Tint1"), color.hues = c(1)) + 
  scale_y_continuous(breaks=c(0, 5, 10)) + 
  coord_flip() +
  theme(panel.grid.minor=element_blank(), 
        panel.grid.major.y=element_blank(),
        axis.ticks = element_blank(), 
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        legend.position="none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank())


ggsave(d2, file="GWCOL_d2.png", width = 80, height = 80, units = "mm", dpi = 300)



#########################################################################
## Simple line plot
#########################################################################

## Using color for emphasis
## Use a darker shade or different colour to highlight/emphasize a focal point.
## Same example but with a line plot as an illustration. 

# make data
df <- data.frame(trt=rep(c("Dose 1", "Dose 2", "Dose 3"), each=4),
                 highlight=rep(c("Y", "N", "Y"), each=4),
                 visit=rep(c(1, 2, 3, 10),3),
                 response=c(0.8, 2.9, 4.2,2.5, 0.4, 2.4,3.8,1.0,0.2, 2.1,3.0,0.2))
#head(df)

# define a minimal theme
th_con <- theme_minimal(base_size = 12 ) +  
  theme(legend.position="none",
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank()
  )    

## Dummy color value
e1 <- ggplot(df, aes(x=visit, y=response, group=trt, color = 'Y')) + 
  geom_line(size=1.5) + ggColor(color.values = "Tint1", color.hues = c(1)) + 
  scale_y_continuous(limits = c(0, 5), breaks = c(0,2.5, 5)) +
  th_con 

ggsave(e1, file="GWCOL_e1.png", width = 80, height = 80, units = "mm", dpi = 300)


## Use highlight dummy value for highlighting the dose
e2 <- ggplot(df, aes(x=visit, y=response, group=trt, color = highlight)) + 
  geom_line(size=1.5) + ggColor(color.values = c("Shade2","Tint1"), color.hues = c(1)) + 
  scale_y_continuous(limits = c(0, 5), breaks = c(0,2.5, 5)) +
  th_con 

ggsave(e2, file="GWCOL_e2.png", width = 80, height = 80, units = "mm", dpi = 300)

#########################################################################
## Simple scatter plot
#########################################################################

## Thinking about gridlines
## Soften gridlines using a light colour such as gray and reduce the size to be less 
## prominent than the data.


set.seed(1)

# Create some data
df <- data.frame(cause=c(rnorm(12,2,2)),
                 effect=c(rnorm(12,2,2))
)

f1 <- ggplot(df, aes(x=cause, y=effect)) + 
  geom_point(size=5) +
  theme_linedraw(base_size=12) +
  theme(legend.position="none",
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        panel.grid.major=element_line(size = 1)
  ) 

ggsave(f1, file="GWCOL_f1.png", width = 80, height = 80, units = "mm", dpi = 300)

f2 <- ggplot(df, aes(x=cause, y=effect)) + 
  geom_point(size=5) +
  theme_minimal(base_size = 12 ) +  
  theme(panel.grid.minor=element_blank(),
        legend.position="none",
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        panel.grid.major=element_line(size = 1, color = rgb(0.75,0.75,0.75))
  ) 


ggsave(f2, file="GWCOL_f2.png", width = 80, height = 80, units = "mm", dpi = 300)



################################################################
## Plot all examples in to a single file
################################################################
all <- grid.arrange(a1, a2, b1, b2, c1, c2, d1, d2, e1, e2, f1, f2, ncol = 4)
ggsave(all, file="GWCOL_all.png", width = 280, height = 220, units = "mm", dpi = 300)
