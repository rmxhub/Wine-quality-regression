library(ggplot2)
library (gridExtra)
library(GGally)

wine<-read.csv('C:\\Users\\Administrator\\Desktop\\UHCL\\linear_regression_4e_data_sets\\projectB19.csv')
colnames(wine)<-c("quality","varietal","pH" ,"sulphates","ClrDensity","WineClr","plyPigmentClr","AnthColr","Anthocyanin","IonDegree","IonAnthocyanin")
myvars = c("quality","pH" ,"sulphates","ClrDensity","WineClr","plyPigmentClr","AnthColr","Anthocyanin","IonDegree","IonAnthocyanin")
myvars = colnames(wine)
plot(wine[myvars[-2]])

cov(wine)
cor(wine) 

summary(wine)
#overall distribution
p5<-ggplot(aes(x=plyPigmentClr),
       data =  wine)+
   geom_histogram(color =I('white'),fill = I('#199009'))+
  ggtitle('Pigment Color distribution')
p6<-ggplot(aes(x=AnthColr),
       data =  wine)+
   geom_histogram(color =I('white'),fill = I('#199009'))+
  ggtitle('Antho-Color distribution')
p7<-ggplot(aes(x=Anthocyanin),
       data =  wine)+
   geom_histogram(color =I('white'),fill = I('#199009'))+
  ggtitle('Anthocyanin distribution')
p8<-ggplot(aes(x=IonDegree),
       data =  wine)+
   geom_histogram(color =I('white'),fill = I('#199009'))+
  ggtitle('Degree of ionized antho distribution')
p9<-ggplot(aes(x=IonAnthocyanin),
       data =  wine)+
   geom_histogram(color =I('white'),fill = I('#199009'))+
  ggtitle('Ionized Anthocyanin distribution')
p0<-ggplot(aes(x=quality),
       data =  wine)+
   geom_histogram(color =I('white'),fill = I('#199009'))+
  ggtitle('Wine Quality')
grid.arrange(p5,p6,p7,p8,p9,p0,ncol=2)

p4<-ggplot(aes(x=WineClr),
       data =  wine)+
   geom_histogram(color =I('white'),fill = I('#199009'))+
  ggtitle('Wine Color distribution')
p1<-ggplot(aes(x=pH),
       data =  wine)+
   geom_histogram(color =I('white'),fill = I('#199009'))+
  ggtitle('pH distribution')
p2<-ggplot(aes(x=sulphates),
       data =  wine)+
   geom_histogram(color =I('white'),fill = I('#199009'))+
  ggtitle('Total SO2 distribution')
p3<-ggplot(aes(x=ClrDensity),
       data =  wine)+
   geom_histogram(color =I('white'),fill = I('#199009'))+
  ggtitle('Color Density distribution')

grid.arrange(p1,p2,p3,p4,ncol=2)
x1.f <- factor(wine$x1)
is.factor(x1.f )
wine.lm<- lm(y~ x1.f+x2+x3+x4+x5+x6+x7+x8+x9+x10, data = wine)
## Determine the number of observations
len = length(wine$y)
#lag plt for independent checking
plot(wine.lm$residuals[2:len],wine.lm$residuals[1:len-1],ylab="Residuals",
     xlab="Lag 1 Residuals",main="Lag Plot")

wine.reduce<- lm(y~ x1.f+x2+x3+x9, data = wine)

#model comparison
library(ordinal)
anova(wine.lm,wine.reduce)

library(rms)
md1 = lrm(y~ x1.f+x2+x3+x4+x5+x6+x7+x8+x9+x10, data = data.frame(scale(wine)), tol=1e-100)
m1 = lm(y~ x1.f+x2+x3+x4+x5+x7+x8+x9, data = wine)
dev.1 = md1$deviance[2]
AIC(m1); BIC(m1);dev.1

md2 = lrm(y~ x1.f+x2+x3+x7+x8, data = wine)
m2 = lm(y~ x1.f+x2+x3+x7+x8, data = wine)
dev.2 = md2$deviance[2]
AIC(m2); BIC(m2);dev.2

md3 = lrm(y~ x1.f+x2+x3+x9, data = wine)
m3 = lm(y~ x1.f+x2+x3+x9, data = wine)
dev.3 = md3$deviance[2]
AIC(m3); BIC(m3);dev.3

md4 = lrm(y~ x1.f+x5, data = wine)
m4 = lm(y~ x1.f+x5, data = wine)
dev.4 = md4$deviance[2]
AIC(m4); BIC(m4);dev.4 

lrtest(md1, md2)
lrtest(md1, md3)
lrtest(md1, md4)
lrtest(md2, md3)
lrtest(md2, md4)
lrtest(md3, md4)
anova(m1,m3)
anova(m2,m3)
anova(m1,m2)
anova(m1,m4)


library(reshape2) library(lattice)
#box plot
# create standized wine
scaled.dat <- scale(wine)
scaled.dat[,2]=wine[,2]
x <- data.frame(
 varietal = rep(scaled.dat[,2],10),
 sample =c(scaled.dat[,1],scaled.dat[,3],scaled.dat[,4],scaled.dat[,5],scaled.dat[,6],
		scaled.dat[,7],scaled.dat[,8],scaled.dat[,9],scaled.dat[,10],scaled.dat[,11]),
 label = rep(c('y', 'x2', 'x3', 'x4','x5', 'x6', 'x7', 'x8', 'x9', 'x10'), , each = 32)
)

bwplot( sample  ~label |varietal , data=x, main="varietal 0 or varietal 1")

par(mfrow=c(3,3),mar=c(2,2,2,1), mgp=c(1.5, 0.5, 0), oma=c(0,0,0,0))
for(i in (1:9) ){
qqnorm(wine[,i], ylab="", 
     xlab="", 
     main=colnames(wine)[i])
qqline(wine[,i])
}
par(mfrow=c(1,2),mar=c(2,2,2,1), mgp=c(1.5, 0.5, 0), oma=c(0,0,0,0))
for(i in (10:11) ){
qqnorm(wine[,i], ylab="", 
     xlab="", 
     main=colnames(wine)[i])
qqline(wine[,i])
}
#scree plot
require(graphics)
(pc.cr <- princomp(wine[,2:11], cor = TRUE))  # inappropriate
screeplot(pc.cr)




#duplex algorithm
library(prospectr)
dup<-duplex(X=wine,k=16)
# k is the number of selected samples

wine.dup.model=wine[dup$model,]
wine.dup.test=wine[dup$test,]

wine.model<- lm(y~ factor(x1)+x2+x3+x4+x5+x6+x7+x8+x9+x10, data = wine.dup.model)
estimatewine=predict(wine.model, wine.dup.model, se.fit = TRUE)
predictwine=predict(wine.model, wine.dup.test, se.fit = TRUE)

plot(wine.dup.test[,1],predictwine$fit, xlab="Wine Quality", 
	ylab="Predicted Quality", col="red",pch = 23)# calibration samples

points(wine.dup.model[,1],estimatewine$fit, col="blue",pch=19)# calibration samples
legend(1,1, "sin(c x)", pch = 21, pt.bg = "white", lty = 1, col = "blue")

plot(predictwine$fit, wine.dup.test[,1]-predictwine$fit, xlab="Fitted Wine Quality", 
	ylab="Residuals", col="red",pch=23)# calibration samples
points(estimatewine$fit, wine.dup.model[,1]-estimatewine$fit,col="blue",pch=19)# calibration samples

wine.lm<- lm(y~ x1+x2+x3+x9, data = wine)
wine.lm$residuals

library(car)
 vif(wine.lm)
influencePlot(wine.lm)
residualPlots(wine.lm)
leveragePlots(wine.lm)

step(wine.lm, direction = c("both"))
step(wine.lm, direction = c("forward"))
step(wine.lm, direction = c("backward"))

dropterm(wine.lm, test = "F")
library(leaps)
leaps( x=Grocery[,2:4], y=Grocery[,1], names=names(Grocery)[2:4], method="Cp")

anova(ols)
drop1(ols)
par(mfrow=c(1,1))
plot(predict(ols),residuals(ols))
par(mfrow=c(3,3),mar=c(3,3,2,1), mgp=c(1.5, 0.5, 0), oma=c(0,0,0,0))
plot(wine$pH,residuals(ols))
plot(wine$sulphates,residuals(ols))
plot(wine$ClrDensity,residuals(ols))
plot(wine$WineClr,residuals(ols))
plot(wine$plyPigmentClr,residuals(ols))
plot(wine$Anthocyanin,residuals(ols))
plot(wine$IonDegree,residuals(ols))
plot(wine$AnthColr,residuals(ols))
plot(wine$IonAnthocyanin,residuals(ols))

qqnorm(residuals(ols))
qqline(residuals(ols))


plot(ols, las = 1)
#Cook's D
d1 <- cooks.distance(ols)
r <- stdres(ols)
a <- cbind(sample, d1, r)
a[d1 > 4/nrow(sample), c("Firsts_2013", "Members", "Age_College", "log_Wine_budget",
    "Percent_female")]


#Influence plot
influencePlot(ols, id.method = "College_char", 
	main = "Influence Plot", sub = "Circle size is proportial to Cook's Distance")

