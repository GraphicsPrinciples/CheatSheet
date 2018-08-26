# Legibility and Clarity

rm(list=ls())
library(ggplot2)
library(dplyr)
library(reshape2)
library(gridExtra)
library(survival)
library(ggfortify)



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



###############################################################################
## Annotated dose response curve 
## Label axes with clear measurement units and 
## provide annotations that support the message.
###############################################################################

## make data
set.seed(12345666)
Dose = seq(0,10,0.1)
DAY = floor(Dose/24)
K1 = 0.2
K2 = 0.8
Response = 100*(Dose/1)/(Dose/1 + 1) 
my.data <- data.frame(Dose = Dose, Response = Response,
                      ymin = Response - 0.1*Response - 5, 
                      ymax = Response + 0.1*Response + 5,
                      obs = Response + 
                        5*rnorm(length(Response)) + 
                        0.1*rnorm(length(Response))*Response,
                      DAY = DAY)

## plot
a1 <- ggplot(data = my.data, aes(x = Dose, y = Response)) + 
  geom_point(data = my.data[Dose%in%c(0.1,1,2.5,5,10),], 
                      aes(x=Dose, y=obs),size = 4) + 
  geom_errorbar(data = my.data[Dose%in%c(0.1,1,2.5,5,10),],  
                aes(x=Dose, ymin=obs-5-0.1*obs, ymax = obs+5+0.1*obs),size = 1) + 
  xlab("Dose") + 
  ylab("Response") + 
  coord_cartesian(ylim = c(-10,120)) + 
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = ymin, ymax=ymax), fill = rgb(0.5,0.5,0.5), alpha = 0.5) +
  geom_point(data = my.data[Dose%in%c(0.1,1,2.5,5,10),], 
             aes(x=Dose, y=obs),size = 4) + 
  geom_errorbar(data = my.data[Dose%in%c(0.1,1,2.5,5,10),],  
                aes(x=Dose, ymin=obs-5-0.1*obs, ymax = obs+5+0.1*obs),size = 1) +
  theme_bw(base_size = 16) + 
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        legend.position="none",
        axis.text.x=element_text(size = 12)
  ) 
  
ggsave(a1, file="GWLeg_a1.png", width = 80, height = 80, units = "mm", dpi = 300)

## Add informative annotations
a2 <- a1 + geom_hline(yintercept = 80, color = "red", linetype = "dashed", size = 1, alpha = 0.4)+
  geom_ribbon(data = data.frame(x=c(0,10), y=c(80,80), ymin = c(70,70), ymax = c(90,90)),
              aes(x = x, y=y, ymin = ymin, ymax = ymax), fill = "red", alpha = 0.2)+
  scale_x_continuous(breaks = c(0.1,1,2.5,5,10), labels = c(0.1,1,2.5,5,10)) +
  geom_line(data = data.frame(x = c(2.5, 8), y = c(-7,-7)), aes(x=x, y=y), color = "red") +
  geom_point(data = data.frame(x = 4, y=-7), aes(x=x, y=y), color = "red", size = 2) +
  annotate("text", label = "Target dose", x = 8, y = 0, color = "red") +
  annotate("text", label = "Active control", x = 8, y = 60, color = "red") +
  xlab("Dose (mg)") + ylab("Response") +
  theme(panel.grid.major=element_line(color = "lightgrey", size = 0.4)) 
  
a2
ggsave(a2, file="GWLeg_a2.png", width = 80, height = 80, units = "mm", dpi = 300)



###############################################################################
## Simple survival plot
## Use font size to create a visual hierarchy
###############################################################################

## data
df <- lung
df$sex <- plyr::mapvalues(df$sex, c(1,2),c("Male","Female"))
df$sex <- factor(df$sex, levels = c("Male","Female"))
fit <- survfit(Surv(time, status) ~ sex, data = df)

b1 <-autoplot(fit, conf.int = FALSE, censor = FALSE, surv.size = 2) + 
  theme_minimal() + 
  ggColor() + 
  labs(title="Survival Analysis") + 
  xlab("Time (years)") + 
  ylab("Survival Rate (%)")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=12),
        legend.title = element_blank(),
        legend.position=c(0.75,0.75),
        legend.text = element_text(size = 12),
        title=element_text(size=8),
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(size = 12)
  ) 
ggsave(b1, file="GWLeg_b1.png", width = 80, height = 80, units = "mm", dpi = 300)


b2 <-autoplot(fit, conf.int = FALSE, censor = FALSE, surv.size = 2) + 
  theme_minimal() + ggColor() + 
  labs(title="Survival Analysis") + 
  xlab("Time (years)") + 
  ylab("Survival Rate (%)")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        #axis.text.y=element_blank(),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=12),
        legend.title = element_blank(),
        legend.position=c(0.75,0.75),
        legend.text = element_text(size = 12),
        title=element_text(face="bold", size=14)
  ) 

ggsave(b2, file="GWLeg_b2.png", width = 80, height = 80, units = "mm", dpi = 300)


####################################################################################
## Simple bar chart
## Don't use too small or too condensed text, Dont capitalize everything
####################################################################################

# create toy data
DF <- data.frame(
  lab = c("C4", "C3", "C2", "C1"),
  value = c(0.05,0.15,0.3,0.5),
  Category = c(4,3,2,1)
)

c1 <- ggplot(DF, aes(reorder(lab,value), value)) +
  geom_bar(width = 0.85, stat = "identity", color = "white") +
  geom_text(aes(x=lab, y=value, label=value),nudge_y=0.1, size=6.5) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(title="Subgroup incidence rate") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.title=element_blank(),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        axis.text.x=element_text(size=18),
        axis.text.y=element_text(size=10),
        legend.position="none",
        title=element_text(face="bold", size=8)
        ) +   
  coord_flip()

ggsave(c1, file="GWLeg_c1.png", width = 80, height = 80, units = "mm", dpi = 300)



c2 <- ggplot(DF, aes(reorder(lab,value), value)) +
  geom_bar(width = 0.65, stat = "identity",color = "white") +
  geom_text(aes(x=lab, y=value, label=value),nudge_y=0.15, size=4) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(title="Subgroup \nincidence rate") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.title=element_blank(),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=12),
        legend.position="none",
        title=element_text(face="bold", size=14)
        )  +   
  coord_flip()

ggsave(c2, file="GWLeg_c2.png", width = 80, height = 80, units = "mm", dpi = 300)


############################################################################
## Simple forest plot
## Use sans serif fonts
############################################################################

# Make data
df <- data.frame(Dose=c(100, 200, 400, 800))
df$Response <- df$Dose/(df$Dose + 200)
df$ymin <- df$Response*exp(0.2)
df$ymax <- df$Response*exp(-0.2)

# plot
d1 <- ggplot(data = df) + 
  geom_point(aes(x = Dose, y = Response), size = 4) + 
  geom_errorbar(aes(x= Dose, ymin = ymin, ymax = ymax)) + 
  scale_y_continuous(limits = c(0, 1)) +
  ggtitle("Dose Reponse") + 
  theme_minimal(base_size = 18) + 
  theme(title=element_text(face="bold",size=18, family="serif"),
        plot.title = element_text(hjust = 0.5),
        axis.title = element_text( family="serif"), 
        axis.text = element_text(family = "serif", size = 12)
        ) 

ggsave(d1, file="GWLeg_d1.png", width = 80, height = 80, units = "mm", dpi = 300)


d2 <- ggplot(data = df) + 
  geom_point(aes(x = Dose, y = Response), size = 4) + 
  geom_errorbar(aes(x= Dose, ymin = ymin, ymax = ymax)) + 
  scale_y_continuous(limits = c(0, 1)) +
  ggtitle("Dose Reponse") + 
  theme_minimal(base_size = 18) +  
  theme(title=element_text(size=18),
        axis.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5)
        ) 

ggsave(d2, file="GWLeg_d2.png", width = 80, height = 80, units = "mm", dpi = 300)


######################################################################################
## Simple bar chart
## Display text with enough contrast to be visible. 
## Favor dark text on light backgrounds over light on dark whenever possible. 
######################################################################################

# create toy data
DF <- data.frame(lab = c("Category 4", "Category 3", "Category 2", "Category 1"),
                 value = c(0.05,0.15,0.3,0.5),
                 Category = c(4,3,2,1)
                 )

e1 <- ggplot(DF[DF$lab!="Category 4",], aes(reorder(Category,lab), value)) +
  geom_bar(width = 0.65, stat = "identity",color = "white") +
  geom_text(aes(x=Category, y=value, label=value*100,fontface="bold"),
            nudge_y=-0.05, size=7, color = "white") +
  scale_y_continuous(limits = c(0, 1)) +
  geom_hline(yintercept = 0, colour = "wheat4", linetype=1, size=0.8)+ 
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text.y=element_blank(),
        axis.text.x = element_blank(),
        axis.title=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.background=element_blank(),
        legend.position="none",
        title=element_text(face="bold", size=14)
        ) 


ggsave(e1, file="GWLeg_e1.png", width = 80, height = 80, units = "mm", dpi = 300)



e2 <- ggplot(DF[DF$lab!="Category 4",], aes(reorder(Category,lab), value)) +
  geom_bar(width = 0.65, stat = "identity",color = "white") +
  geom_text(aes(x=Category, y=value, label=value*100),
            nudge_y=0.05, size=7, color = "black") +
  scale_y_continuous(limits = c(0, 1)) +
  geom_hline(yintercept = 0, colour = "wheat4", linetype=1, size=0.8) + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text.y=element_blank(),
        axis.text.x = element_blank(),
        axis.title=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.background=element_blank(),
        legend.position="none",
        title=element_text(face="bold", size=14)
        ) 


ggsave(e2, file="GWLeg_e2.png", width = 80, height = 80, units = "mm", dpi = 300)


##########################################################################################
## Simple scatter plot
## Bold and italics should only be used for layering. 
## Emphasizing everything means nothing gets emphasized.
##########################################################################################

## make data
set.seed(12345666)
my_data <- data.frame(Bodyweight = 90 + 20*c(-runif(50), runif(50)))
my_data$Exposure <- ((my_data$Bodyweight/90)^-0.75)*exp(0.1*rnorm(length(my_data$Bodyweight)))
my_data$BODYWEIGHT = my_data$Bodyweight
my_data$EXPOSURE <- my_data$Exposure


f1 <- ggplot(my_data, aes(x = BODYWEIGHT, y = EXPOSURE)) + 
  geom_smooth(method = "lm")+
  geom_point() + 
  theme_minimal(base_size = 18 ) +  
  ggtitle("EXPOSURE \nVS BODYWEIGHT") +
  theme(panel.grid.minor=element_blank(),
        legend.position="none",
        axis.ticks = element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_blank(),
        axis.title = element_text(face = "bold.italic",size = 18),
        plot.title = element_text(hjust = 0.5, face = "bold.italic",size = 18)
  )

ggsave(f1, file="GWLeg_f1.png", width = 80, height = 80, units = "mm", dpi = 300)



f2 <- ggplot(my_data, aes(x = Bodyweight, y = Exposure)) + 
  geom_smooth(method = "lm")+
  geom_point() + 
  theme_minimal(base_size = 16 ) +  
  ggtitle("Exposure \nvs. Bodyweight")+
  theme(panel.grid.minor=element_blank(),
        legend.position="none",
        axis.ticks = element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_blank(),
        plot.title = element_text(hjust = 0.5)
        )   

ggsave(f2, file="GWLeg_f2.png", width = 80, height = 80, units = "mm", dpi = 300)



######################################################################################
## Simple bar chart
## Try not to set types at an angle to avoid clashing as this decreases legibility. 
## First think of alternative solutions such as transposing the graph, abbreviations, 
## or reducing the number of labels to what is essential.
########################################################################################

# make data
df <- data.frame(trt=c("Treatment 1", "Treatment 2", "Treatment 3"),
cause=c(4,6,10),
highlight = c(2,1,2))

g1 <- ggplot(df, aes(x=trt, y=cause))  +
  geom_bar(width=0.5,  stat = "identity") +  
  theme(axis.title=element_blank()) +
  scale_y_continuous(breaks=c(0, 5, 10))

ggsave(g1, file="GWLeg_g1.png", width = 80, height = 80, units = "mm", dpi = 300)


g2 <- ggplot(df, aes(x=trt, y=cause))  +
  geom_bar(width=0.5,  stat = "identity") +  
  theme(axis.title=element_blank()) +
  scale_y_continuous(breaks=c(0, 5, 10))  +   
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y=element_text(size=12))

ggsave(g2, file="GWLeg_g2.png", width = 80, height = 80, units = "mm", dpi = 300)


g3 <- ggplot(df, aes(x=trt, y=cause)) +
  geom_bar(width=0.5,  stat = "identity") +  
  theme(axis.title=element_blank(),
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12)) +
  scale_y_continuous(breaks=c(0, 5, 10)) + 
  coord_flip() 

ggsave(g3, file="GWLeg_g3.png", width = 80, height = 80, units = "mm", dpi = 300)

