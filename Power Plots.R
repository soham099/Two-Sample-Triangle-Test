data1<- cbind(Dimension=c(50,75,100,200,300,400,500), Power=c(0.132,0.2,0.202,0.346,0.48,0.538,0.666))
data1<-data.frame(data1)
library(ggplot2)
ggplot(data1, aes(x=Dimension, y=Power))+geom_point()+geom_line()+ylim(c(0,1))


data2<- cbind(Dimension=rep(c(10,11,12,13),2), Power=c(0.474,0.532,0.612,0.628,0.942,0.97,0.974,0.99), 
                      Test=(c(rep("CvM",4), rep("$L^2$-CvM",4))))

data2<-data.frame(data2)

data2$Power<-as.numeric(data2$Power)
data2$Dimension<-as.numeric(data2$Dimension)

ggplot(data2, aes(x=Dimension, y=Power, colour=Test))+geom_point()+geom_line()+ylim(c(0,1))

data3 <- data.frame(Dimension=rep(c(50,100,200,300,400,500),3), Power=c(0.132,0.202,0.346,0.48,0.538,0.666, 0.98,0.99,0.99,1,1,1,
                                                                        0.044,0.052,0.058,0.06,0.074,0.081), Test=c(rep("CvM",6), rep("$L^2$-CvM",6), rep("Energy",6)))

data3$Power<-as.numeric(data3$Power)
data3$Dimension<-as.numeric(data3$Dimension)

ggplot(data3, aes(x=Dimension, y=Power, colour=Test))+geom_point()+geom_line()+ylim(c(0,1))
