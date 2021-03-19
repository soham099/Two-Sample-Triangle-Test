data1<- cbind(Dimension=c(50,75,100,200,300,400,500), Power=c(0.132,0.2,0.202,0.346,0.48,0.538,0.666))
data1<-data.frame(data1)
library(ggplot2)
ggplot(data1, aes(x=Dimension, y=Power))+geom_point()+geom_line()+ylim(c(0,1))


data2<- cbind(Dimension=rep(c(10,11,12,13),2), Power=c(0.474,0.532,0.612,0.628,0.942,0.97,0.974,0.99), 
                      Test=(c(rep("CvM",4), rep("$L^2$-CvM",4))))

data2<-data.frame(data2)

data2$Power<-as.numeric(data2$Power)
data2$Dimension<-as.numeric(data2$Dimension)

ggplot(data2, aes(x=Dimension, y=Power, colour=Test))+geom_point(shape=17, size=3)+geom_line()+ylim(c(0,1))+
  scale_color_discrete(labels = c(expression(L^2-CvM), expression(CvM)))+
  theme(axis.text.x=element_text(size=14, vjust=0.5),
        axis.text.y=element_text(size=14, hjust=0.5),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "grey"), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA),
        legend.text = element_text(size = 12), 
        legend.text.align = NULL, 
        legend.title = element_text(size = 13, face = "bold", hjust = 0))+scale_color_brewer(palette="Dark2")

data3 <- data.frame(Dimension=rep(c(50,100,200,300,400,500),3), Power=c(0.132,0.202,0.346,0.48,0.538,0.666, 0.98,0.99,0.99,1,1,1,
                                                                        0.044,0.052,0.058,0.06,0.074,0.081), Test=c(rep("CvM",6), rep("$L^2$-CvM",6), rep("Energy",6)))

data3$Power<-as.numeric(data3$Power)
data3$Dimension<-as.numeric(data3$Dimension)

ggplot(data3, aes(x=Dimension, y=Power, colour=Test))+geom_point(shape=17, size=3)+geom_line()+ylim(c(0,1))+
  scale_color_discrete(labels = c(expression(L^2-CvM), expression(CvM),expression(Energy)))+
  theme(axis.text.x=element_text(size=14, vjust=0.5),
        axis.text.y=element_text(size=14, hjust=0.5),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "grey"), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA),
        legend.text = element_text(size = 12), 
        legend.text.align = NULL, 
        legend.title = element_text(size = 13, face = "bold", hjust = 0))


data4<- cbind(Dimension=c(50,100,200,300,400,500), Power=c(0.15,0.144,0.16,0.18,0.152,0.154))
data4<- data.frame(data4)

ggplot(data4, aes(x=Dimension, y=Power))+geom_point(shape=17,size=3)+geom_line()+ylim(c(0,1))+ 
  theme(axis.text.x=element_text(size=14, vjust=0.5),
        axis.text.y=element_text(size=14, hjust=0.5),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "grey"), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA))



data5<- cbind(Dimension=c(50,100,200,300,400,500), Power=c(0.048,0.058,0.066,0.044,0.06,0.058))
data5<- data.frame(data5)

ggplot(data5, aes(x=Dimension, y=Power))+geom_point(shape=17,size=3)+geom_line()+ylim(c(0,1))+ 
  theme(axis.text.x=element_text(size=14, vjust=0.5),
        axis.text.y=element_text(size=14, hjust=0.5),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "grey"), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA))




data6 <- data.frame(Dimension=rep(c(50,100,200,300,400,500),4), Power=c(
                                                                        0.15,0.144,0.16,0.18,0.152,0.154, 0.052,0.182,0.552,0.89,0.978,0.996,
                                                                        0.048,0.058,0.066,0.044,0.06,0.058, 0.048,0.038,0.062,0.058,0.044,0.058
                                                                        ), Test=c(rep("L^2-CvM",6),rep("phi-L^2-CvM",6), rep("CvM",6), rep("Energy",6)))

data6$Power<-as.numeric(data6$Power)
data6$Dimension<-as.numeric(data6$Dimension)

ggplot(data6, aes(x=Dimension, y=Power, colour=Test))+geom_point(shape=17, size=3)+geom_line()+ylim(c(0,1))+
  scale_color_discrete(labels = c( expression(CvM), expression(Energy), expression(L^2-CvM), expression(phi-L^2-CvM)))+
  theme(axis.text.x=element_text(size=14, vjust=0.5),
        axis.text.y=element_text(size=14, hjust=0.5),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14,angle=90),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "grey"), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA),
        legend.text = element_text(size = 12), 
        legend.text.align = 0.1, 
        legend.title = element_text(size = 13, face = "bold", hjust = 0),
        plot.margin = unit(c(1, 1.5, 0.5, 0.5), "lines"), complete = TRUE)


