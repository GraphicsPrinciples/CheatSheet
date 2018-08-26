# Implementation Considerations

library(ggplot2)
library(graphics)
library(ggplot2)
library(MASS)
library(scales) #scale_x_date()
library(gridExtra) 

## Set theme
theme_set(theme_minimal(base_size=18))
th <- theme(panel.grid.minor=element_blank(),
            panel.grid.major=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            strip.background = element_rect(fill = "lightgrey", colour = "lightgrey"),
            panel.background = element_rect(fill = "white", colour = "lightgrey")
)


############################################################
## Simple scatter plot - cause and effect 
############################################################
## Cause and effect

# For xy plots display the cause on the x-axis and the effect on the y-axis. 
set.seed(12345666)
my_data <- data.frame(
  Bodyweight = 90 + 20*c(-runif(50), runif(50))
)
my_data$AUC <- ((my_data$Bodyweight/90)^-0.75)*exp(0.1*rnorm(length(my_data$Bodyweight)))

ggplot(my_data, aes(x = AUC, y = Bodyweight)) + 
  geom_smooth(method = "lm") +
  geom_point() + 
  theme_minimal(base_size = 18 ) +  
  theme(panel.grid.minor=element_blank(),
        #panel.grid.major=element_blank(),
        legend.position="none",
        axis.ticks = element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_blank(),
        panel.background = element_rect(fill = "white", colour = "lightgrey")
  )    

ggsave(file="GWImp_a1.png", width = 80, height = 80, units = "mm", dpi = 300)

ggplot(my_data, aes(x = Bodyweight, y = AUC)) + 
  geom_smooth(method = "lm")+
  geom_point() + theme_minimal(base_size = 18 ) +  
  theme(panel.grid.minor=element_blank(),
        #panel.grid.major=element_blank(),
        legend.position="none",
        axis.ticks = element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_blank(),
        panel.background = element_rect(fill = "white", colour = "lightgrey")
  )    

ggsave(file="GWImp_a2.png", width = 80, height = 80, units = "mm", dpi = 300)


###################################################################
## Simple scatter plot - Aspect ratio
####################################################################

# The selection of the aspect ratio can influence the interpretation of a line graph. 
# Try to display lesser height versus greater length. 
# Aim to scale the representation to have approximately a 45 degree angle of change.

set.seed(12345666)
my_data <- data.frame(
  x = 50*rnorm(1000)
)
my_data$y <- (0.2*my_data$x+0.8*50*rnorm(length(my_data$x)))

p <- ggplot(my_data, aes(x = x, y = y)) +
  geom_point() + theme_minimal(base_size = 18 ) +  
  theme_bw(base_size=16) +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        legend.position="none",
        axis.title.y=element_blank(),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_blank()
  )    
p + coord_fixed(0.5) 
ggsave(file="GWImp_b1.png", width = 80, height = 80, units = "mm", dpi = 300)

p + coord_fixed(ratio=3) 
ggsave(file="GWImp_b2.png", width = 80, height = 80, units = "mm", dpi = 300)



################################################################
## Simple bar chart and dot plot
################################################################

## Plotting bars on a non-linear axis
## Avoid using comparisons based on length (i.e. a bar chart) 
## when displaying data on a non-linear scale (i.e. a log scale or percentage change). 
## Instead display comparisons using position (i.e.a dot chart). 


ggplot(OrchardSprays,aes(x=treatment,y=decrease)) +
  stat_summary(fun.data=mean_cl_normal,geom="bar") +
  theme_minimal(base_size = 18 )+
  scale_y_log10(limits = c(1, 100), breaks = c(1,10,100)) +
  xlab("Treatment") +
  ylab("Effect")

ggsave(file="GWImp_c1.png", width = 80, height = 80, units = "mm", dpi = 300)


mm1 <- function(...) {
  mean_cl_normal(...,mult=1)
}
mm2 <- function(...) {
  mean_cl_normal(...,mult=2)
}

ggplot(OrchardSprays,aes(x=treatment,y=decrease)) +
  stat_summary(fun.data=mm1,geom="point", color = "black",size=4) +
  theme_minimal(base_size = 18)+
  scale_y_log10(limits = c(1, 100), breaks = c(1,10,100))+
  xlab("Treatment")+
  ylab("Effect")

ggsave(file="GWImp_c2.png", width = 80, height = 80, units = "mm", dpi = 300)


######################################################
## Simple dot plot including uncertainty estimates
######################################################
## Log Scales
## Do not plot log-normally distributed variables on a linear scale 
## (e.g. hazards ratio, odds ratio, AUC, CL)

DF <- data.frame(Est = c(1,2,3,4,5), 
                 Point = c(-0.7, 0.7, 0.2, 0.1, -0.3),
                 SE = c(0.1,0.1,0.1,0.8,0.9))

gg <- ggplot(data = DF) + 
  geom_point(aes(x=Est, y= exp(Point)), size = 3) + 
  geom_errorbar(aes(x = Est, ymin = exp(Point-2*SE), 
                ymax = exp(Point+2*SE)), width = 0.5) + 
  scale_y_continuous(breaks = c(0,1,2,3,4,5,6,7,8)) + 
  geom_hline(yintercept = 1, linetype = "dashed", color = rgb(0.5,0.5,0.5)) +
  geom_vline(xintercept = 0, linetype = "solid", color = "black") +
  ylab("Odds Ratio") + 
  theme_minimal(base_size = 18 ) +  
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        legend.position="none",
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x=element_blank()
        )    

gg
ggsave(file="GWImp_d1.png", width = 80, height = 80, units = "mm", dpi = 300)

gg + scale_y_log10(breaks = c(0.125,0.25,0.5,1,2,4,8), labels = c("1/8","1/4","1/2","1","2","4","8")) + 
  geom_hline(yintercept = 1, linetype = "dashed", color = rgb(0.5,0.5,0.5))

ggsave(file="GWImp_d2.png", width = 80, height = 80, units = "mm", dpi = 300)


#############################################################################
## Simple panelled line plot
#############################################################################
## Consistent scales

set.seed(12345666)
Time = c(c(0.5,1,2,4,8,12,18,23.99), 3*24 + c(0.5,1,2,4,8,12,18,23.99))
DAY = floor(Time/24)
DOSE = (1+DAY)*100
K1 = 0.2
K2 = 0.8
Concentration = DOSE*(K2*K1/(K2-K1))*(exp(-K1*(Time-DAY*24)) - exp(-K2*(Time-DAY*24)) )
my.data <- data.frame(Time = Time, Concentration = Concentration, DAY = DAY, DOSE = DOSE)

gg <- ggplot(data = my.data, aes(x = Time-DAY*24, y = Concentration)) + 
  geom_point(size = 4) + 
  geom_line(aes(group = DAY), size = 1) + scale_y_log10() + 
  theme_minimal(base_size = 18) + 
  theme(panel.grid.minor=element_blank(),
                 panel.grid.major=element_blank(),
                 legend.position="none",
                 axis.title.y=element_blank(),
                 axis.title.x=element_blank(),
                 axis.ticks = element_blank(),
                 strip.background = element_rect(fill = "lightgrey", colour = "grey50"),
                 panel.background = element_rect(fill = "white", colour = "grey50"),
                 axis.text.x=element_blank())   

gg + facet_wrap(~DOSE, scales = "free_y")
ggsave(file="GWImp_e1.png", width = 80, height = 80, units = "mm", dpi = 300)

gg + facet_wrap(~DOSE)
ggsave(file="GWImp_e2.png", width = 80, height = 80, units = "mm", dpi = 300)


#########################################################################
## Simple line plot with disconnected trend
#########################################################################
## Disconnected scale

set.seed(12345666)
Time = c(c(0,0.5,1,2,4,8,12,18,23.99), 24 + seq(1,2,1)*24, 3*24 + c(0,0.5,1,2,4,8,12,18,23.99))
DAY = floor(Time/24)
K1 = 0.2
K2 = 0.8
Concentration = 100*(K2*K1/(K2-K1))*(exp(-K1*(Time-DAY*24)) - exp(-K2*(Time-DAY*24)) + 0.25*Time/100/(Time/100 + 1)) 
my.data <- data.frame(Time = Time, Concentration = Concentration, DAY = DAY)

gg <- ggplot(data = my.data, aes(x = Time, y = Concentration)) + 
  geom_point(size = 4) + 
  geom_line(aes(group = DAY), size = 1) + 
  scale_y_log10(lim = c(0.1,100)) + 
  theme_minimal(base_size = 18) + 
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        panel.background = element_rect(fill = "white", colour = "lightgrey"),
                 legend.position="none",
                 axis.title.y=element_blank(),
                 axis.title.x=element_blank(),
                 axis.ticks = element_blank(),
                 axis.text.y=element_blank(),
                 axis.text.x=element_blank()
)  

gg + geom_line(size = 1)
ggsave(file="GWImp_f1.png", width = 80, height = 80, units = "mm", dpi = 300)

gg
ggsave(file="GWImp_f2.png", width = 80, height = 80, units = "mm", dpi = 300)


##############################################################
## Simple line plot with uncertainty intervals and band
##############################################################
## Inferences

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

gg <- ggplot(data = my.data, aes(x = Dose, y = Response)) + 
  theme_minimal(base_size = 18) + 
  geom_line(size = 1) + 
  coord_cartesian(ylim = c(-10,120)) +
  theme(panel.grid.minor=element_blank(),
                 panel.grid.major=element_blank(),
                 legend.position="none",
                 axis.title.y=element_blank(),
                 axis.title.x=element_blank(),
                 axis.ticks = element_blank(),
                 axis.text.y=element_blank(),
                 axis.text.x=element_blank(),
                 panel.background = element_rect(fill = "white", colour = "lightgrey")
        )  
gg

ggsave(file="GWImp_g1.png", width = 80, height = 80, units = "mm", dpi = 300)

gg + geom_ribbon(aes(ymin = ymin, ymax=ymax), fill = rgb(0.5,0.5,0.5), alpha = 0.5) +
  geom_point(data = my.data[Dose%in%c(0.1,1,2.5,5,10),], 
             aes(x=Dose, y=obs),size = 4) + 
  geom_errorbar(data = my.data[Dose%in%c(0.1,1,2.5,5,10),],  
                aes(x=Dose, ymin=obs-5-0.1*obs, ymax = obs+5+0.1*obs),size = 1) 

ggsave(file="GWImp_g2.png", width = 80, height = 80, units = "mm", dpi = 300)


#############################################################
## Simple boxplots on a linear scale
#############################################################
## Continuous Time Scales

grl <- data.frame(x=rep(c(0,1,2,4,8,12,24),200))
grl$y <- 100*(exp(-0.5*grl$x+0.5*rnorm(length(grl$x)))+0.5*rnorm(length(grl$x)))
ggplot(grl, aes(x=factor(x),y=y)) + 
  geom_boxplot(aes(group = factor(x)),size=1, color = "black", outlier.size = 3) + 
  theme_minimal(base_size=26) + 
  th

ggsave(file="GWImp_h1.png", width = 80, height = 80, units = "mm", dpi = 300)


ggplot(grl, aes(x=x,y=y)) + 
  geom_boxplot(aes(group = factor(x)),size=1, color = "black", outlier.size = 3) + 
  theme_minimal(base_size=26) + 
  th

ggsave(file="GWImp_h2.png", width = 80, height = 80, units = "mm", dpi = 300)
