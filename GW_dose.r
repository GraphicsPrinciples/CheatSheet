# Putting it all together
library(ggplot2)

set.seed(12345666)
Time = rep(c(0.5,1,2,4,8,12,18,24))
N = 6
DOSE = 100*exp(0.4*rnorm(N))
ID = sort(rep(seq(1,N,1),length(Time)))
Time <- rep(Time, N)
K1 = 0.1*exp(0.2*rnorm(N))
K2 = 0.8*exp(0.4*rnorm(N))

my.data <- data.frame(ID=seq(1,N,1), K1, K2, DOSE)
my.data <- merge(my.data, data.frame(Time = Time, ID=ID))

my.data$Concentration = my.data$DOSE*(my.data$K2*my.data$K1/(my.data$K2-my.data$K1))*(exp(-my.data$K1*(my.data$Time)) - exp(-my.data$K2*(my.data$Time)) )*exp(0.3*rnorm(length(my.data$Time))) + 0.2*rnorm(length(my.data$Time))

my.data2 <- data.frame(my.data, ID2 = 1)
for(id in 2:N){
  my.data2 <- rbind(my.data2, data.frame(my.data, ID2 = id))
}

temp <- my.data2$ID
my.data2$ID = my.data2$ID2
my.data2$ID2 <- temp

gg <- ggplot(data = my.data, aes(x = Time, y = Concentration)) + theme_gray(base_size = 14)
gg <- gg + geom_line(aes(group = ID, color = factor(ID), linetype = factor(ID)), size = 1) 
gg <- gg + geom_point(aes(color = factor(ID), shape = factor(ID)), size = 4)
gg <- gg + scale_y_log10() + ggColor()
gg <- gg + theme(  legend.position="none"  )   
gg

ggsave(file="Plot41a.png", width = 80, height = 80, units = "mm", dpi = 300)


gg <- ggplot(data = my.data, aes(x = Time, y = Concentration)) + theme_minimal(base_size = 14)
gg <- gg + geom_line(aes(group = ID, linetype = factor(ID)), size = 1) 
gg <- gg + scale_y_log10() + ggColor()+ xlab("Time (hours)") + ylab("Concentration (ng/mL)")
gg <- gg + theme(panel.grid.minor=element_blank(),        legend.position="none"  )   
gg

ggsave(file="Plot41b.png", width = 80, height = 80, units = "mm", dpi = 300)


gg <- ggplot(data = my.data2, aes(x = Time, y = Concentration)) + theme_minimal(base_size = 14)
gg <- gg + geom_line(aes(group = ID2), size = 1, color = rgb(0.75,0.75,0.75)) + scale_y_log10() + ggColor()
gg <- gg + geom_line(data = my.data, aes(x = Time, y = Concentration,group = ID), size = 1)+ 
  scale_x_continuous(breaks = c(12,24)) + xlab("Time (hours)") + ylab("Concentration (ng/mL)")
gg <- gg + theme(panel.grid.minor=element_blank(),
                 legend.position="none",
                 strip.text.x = element_blank()
)   
gg + facet_wrap(~ID)


ggsave(file="Plot41c.png", width = 80, height = 80, units = "mm", dpi = 300)


