require(graphics)
library(ggplot2)
library(ggthemes)
library(MASS)
library(gridExtra)

mm1 <- function(...) {
  mean_cl_normal(...,mult=1)
}
mm2 <- function(...) {
  mean_cl_normal(...,mult=2)
}


g1 <- ggplot(OrchardSprays,aes(x=treatment,y=decrease, fill = treatment))+
  theme_gray(base_size = 12)+
  scale_y_continuous(limits = c(0,100), breaks = c(0,25,50,75,100)) +
  stat_summary(fun.data=mean_cl_normal,geom="errorbar",width=0.5) +
  stat_summary(fun.data=mean_cl_normal,geom="bar") + ggFill(color.value = "Hue")+ 
  theme(legend.position="none",
        panel.grid.minor=element_line(color = "black"),
        panel.grid.major  = element_line(color = "black"),
        panel.border=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank()
  )

g2 <- ggplot(OrchardSprays,aes(x=treatment,y=decrease)) + theme_minimal(base_size = 14)+
  scale_y_continuous(limits = c(0,100), breaks = c(0,25,50,75,100)) +
  stat_summary(fun.data=mean_cl_normal,geom="errorbar",width=0.5) +
  stat_summary(fun.data=mean_cl_normal,geom="bar") + 
  theme(legend.position="none",
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank()
  ) 

g3 <- ggplot(OrchardSprays,aes(x=treatment,y=decrease)) + theme_minimal(base_size = 14)+
  scale_y_continuous(limits = c(0,100), breaks = c(0,25,50,75,100)) +
  stat_summary(fun.data=mean_cl_normal,geom="errorbar",width=0.5) +
  stat_summary(fun.data=mean_cl_normal,geom="bar",width=0.7) + 
  theme(legend.position="none",
        panel.grid.minor=element_blank(),
        panel.grid.major=element_line(size = 0.5, color = "gray96"),
        panel.background=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank()
  ) 

g4 <- ggplot(OrchardSprays,aes(x=treatment,y=decrease)) + theme_minimal(base_size = 14)+
  scale_y_continuous(limits = c(0,100), breaks = c(0,25,50,75,100)) +
  stat_summary(fun.data=mean_cl_normal,geom="errorbar",width=0.5, size = 1) +
  stat_summary(fun.data=mean_cl_normal,geom="point",size=3) + 
  xlab("Treatment") + ylab("Effect") + 
  theme(legend.position="none",
        panel.grid.minor=element_blank(),
        panel.grid.major=element_line(size = 0.5, color = "gray96"),
        panel.background=element_blank()
  ) 



g5 <- ggplot(OrchardSprays,aes(x=treatment,y=decrease))+
  scale_y_continuous(limits = c(0,100), breaks = c(0,25,50,75,100)) +
  stat_summary(fun.data=mm2,geom="linerange", size = 0.7)+      
  stat_summary(fun.data=mean_cl_normal,geom="point") + 
  xlab("Treatment") + ylab("Effect") + 
  theme(legend.position="none",
        panel.grid.minor=element_blank(),
        panel.grid.major=element_line(size = 0.5, color = "gray96"),
        panel.background=element_blank()
  )  



g6 <- ggplot(OrchardSprays,aes(x=treatment,y=decrease))+
  scale_y_continuous(limits = c(0,100), breaks = c(0,25,50,75,100)) +
  stat_summary(fun.data=mm2,geom="linerange", size = 0.7)+      
  stat_summary(fun.data=mm1,geom="linerange",size=1.1, color = "red", position=position_dodge(width=0))+
  stat_summary(fun.data=mean_cl_normal,geom="point") + 
  xlab("Treatment") + ylab("Effect") + 
  theme(legend.position="none",
        panel.grid.minor=element_blank(),
        panel.grid.major=element_line(size = 0.5, color = "gray96"),
        panel.background=element_blank()
  )  



g1 
ggsave(file="Plot42a.png", width = 80, height = 80, units = "mm", dpi = 300)

g2 
ggsave(file="Plot42b.png", width = 80, height = 80, units = "mm", dpi = 300)

g3 
ggsave(file="Plot42c.png", width = 80, height = 80, units = "mm", dpi = 300)

g4 
ggsave(file="Plot42d.png", width = 80, height = 80, units = "mm", dpi = 300)

g5 
ggsave(file="Plot42e.png", width = 80, height = 80, units = "mm", dpi = 300)

g6 
ggsave(file="Plot42f.png", width = 80, height = 80, units = "mm", dpi = 300)

