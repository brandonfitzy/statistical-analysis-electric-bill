electric=read.table(file="~/desktop/r files/final project/cleanedelectricbilldata.txt", header=T)
attach(electric)

#section 1
electric
mean(Year)
var(Year)
sd(Year)
mean(AveTemp)
var(AveTemp)
sd(AveTemp)
mean(HeatDeg)
var(HeatDeg)
sd(HeatDeg)
mean(CoolDeg)
var(CoolDeg)
sd(CoolDeg)
mean(FamNum)
var(FamNum)
sd(FamNum)
mean(KWH)
var(KWH)
sd(KWH)

#correlations
cor(KWH,Year)
cor(KWH,AveTemp)
cor(KWH,HeatDeg)
cor(KWH,CoolDeg)
cor(KWH,FamNum)
cor(Year,AveTemp)
cor(Year,HeatDeg)
cor(Year,CoolDeg)
cor(Year,FamNum)
cor(AveTemp,HeatDeg)
cor(AveTemp,CoolDeg)
cor(AveTemp,FamNum)
cor(HeatDeg,CoolDeg)
cor(HeatDeg,FamNum)
cor(CoolDeg,FamNum)

#histogroms
par(mfrow=c(3,2))
hist(Year,breaks=c(0,1,2,3,4,5,6,7,8,9,10),
     main="Histogram of Year",xlab="Year (1991 through 2000 (1=1991)")
hist(AveTemp, main="Histogram of Average Temperature",
     xlab = "Average Temperature of the Month")
hist(HeatDeg, main = "Histogram of Heating Degrees",
     xlab = "Total Degrees Heated for the Month")
hist(CoolDeg, main = "Histogram of Cooling Degrees",
     xlab = "Total Degrees Cooled for the Month")
hist(FamNum,breaks=c(1,2,3,4), main = "Histogram of Family Size",
     xlab = "Number of People in the Family")
hist(KWH, main = "Histogram of Kilowatts",
     xlab = "Kilowatts used for the Month")

#boxplot
par(mfrow=c(2,3))
boxplot(AveTemp,data=electric, 
        main="Boxplot of Average Temperature",
        ylab="Average Temperature")
boxplot(HeatDeg,data=electric, 
        main="Boxplot of Heating Degrees",
        ylab="Degrees Heated")
boxplot(CoolDeg,data=electric, 
        main="Boxplot of Cooling Degrees",
        ylab="Degrees Cooled")
boxplot(FamNum,data=electric, 
        main="Boxplot of Family Size",
        ylab="Size of Family")
boxplot(KWH,data=electric, 
        main="Boxplot of Kilowatts",
        ylab="KWH")
boxplot(Year,data=electric,
        main="Boxplot of Year",
        ylab="Year")

#proportions barplot of the categorical variables
proportions=rbind(c(0,0,0,0,0,0,0,0,0,4/120),
                  c(0,0,0,0,0,0,4/120,8/120,8/120,7/120),
                  c(12/120,12/120,12/120,12/120,12/120,12/120,8/120,4/120,4/120,1/120)
                  )
par(mfrow=c(1,1))
barplot(proportions,names.arg=c("1991","1992","1993","1994","1995","1996","1997","1998","1999","2000"),
        legend=c("2","3","4"), ylab="Proportion of Family Size", xlab="Year",
        main="Barplot of Proportion of Family Size by Year")

par(mfrow=c(1,1))
pairs(electric)

par(mfrow=c(2,3))
plot(KWH, Year, main="Plot of Year by KWH", ylab="Year")
abline(lm(Year~KWH))
plot(KWH, AveTemp, main="Plot of Average Temp by KWH", ylab="Average Temperature")
abline(lm(AveTemp~KWH))
plot(KWH, HeatDeg, main="Plot of Heating Degrees by KWH", ylab="Heating Degrees")
abline(lm(HeatDeg~KWH))
plot(KWH, CoolDeg, main="Plot of Cooling Degrees by KWH", ylab="Cooling Degrees")
abline(lm(CoolDeg~KWH))
plot(KWH, FamNum, main="Plot of Family Size by KWH", ylab="Family Size")
abline(lm(FamNum~KWH))

#section 2
#part a
#categorical data independence test
table=table(FamNum,Year)
chisq.test(table)
chisq.test(table)$expected

#part b
#simple linear regression
tempmod=lm(KWH~AveTemp,data=electric)
sum1=summary(tempmod)
confint1=confint(tempmod)
predvalues1=fitted(tempmod)
resid1=residuals(tempmod)

#residual plots
par(mfrow=c(2,2))
qqnorm(resid1, main="QQ Plot of Residuals")
hist(resid1,xlab="Residuals",main="Histogram of Residuals")
plot(Obs,resid1,main="Residuals by Order",xlab="Order",ylab="Residuals")
lines(Obs,resid1)
abline(0,0)

#part c
#multiple linear model
#foreward step wise addition
mod1=lm(KWH~Year+FamNum+HeatDeg+CoolDeg+AveTemp)
sum1=summary(mod1)
#remove year
mod2=lm(KWH~FamNum+HeatDeg+CoolDeg+AveTemp)
sum2=summary(mod2)
#remove heatdeg
mod3=lm(KWH~FamNum+CoolDeg+AveTemp)
sum3=summary(mod3)
#remove cooldeg
mod4=lm(KWH~FamNum+AveTemp)
sum4=summary(mod4)
Res1 <- residuals(bestfit);
Pred1 <- predict(bestfit);
#Create residual plots;
par(mfrow=c(1,2))
Figurec1 <- plot(x=Pred1,y=Res1,
                 main="Residual Plot for Predicted KWT",
                 xlab="Predicted KWT",
                 ylab="Residuals"
);
abline(a=0,b=0);

Figurec2 <- hist(Res1,main="Histogram of the Residuals",
                 xlab="Residuals",
                 ylab="Frequency")
#part d
#one way anova
Year <- as.factor(Year);
FamNum <- as.factor(FamNum);
Figured1 <- boxplot(KWH~Year,data=electric, main="Boxplots of KWH by Year",xlab="Year",ylab="KWH");
Figured2 <- boxplot(KWH~FamNum, data=electric, main="Boxplots of KWH by Family Size",xlab="Family Size",ylab="KWH")
testd1 <- aov(KWH~Year)
sumd1 <-summary(testd1)
MuHatd1 = c(mean(KWH[Year==1]),mean(KWH[Year==2]),mean(KWH[Year==3]),
          mean(KWH[Year==4]),mean(KWH[Year==5]),mean(KWH[Year==6]),
          mean(KWH[Year==7]),mean(KWH[Year==8]),mean(KWH[Year==9]),
          mean(KWH[Year==10]));
sd1 = c(sd(KWH[Year==1]),sd(KWH[Year==2]),sd(KWH[Year==3]),
      sd(KWH[Year==4]),sd(KWH[Year==5]),sd(KWH[Year==6]),
      sd(KWH[Year==7]),sd(KWH[Year==8]),sd(KWH[Year==9]),
      sd(KWH[Year==10]));

sESTd1<-sqrt(4707937)

SpSquared1 <- matrix(0,10,10);
SpSquared1[1,1] = sESTd1*sESTd1/12;
SpSquared1[2,2] = sESTd1*sESTd1/12;
SpSquared1[3,3] = sESTd1*sESTd1/12;
SpSquared1[4,4] = sESTd1*sESTd1/12;
SpSquared1[5,5] = sESTd1*sESTd1/12;
SpSquared1[6,6] = sESTd1*sESTd1/12;
SpSquared1[7,7] = sESTd1*sESTd1/12;
SpSquared1[8,8] = sESTd1*sESTd1/12;
SpSquared1[9,9] = sESTd1*sESTd1/12;
SpSquared1[10,10] = sESTd1*sESTd1/12;
#too many to choose

TukeyHSD(testd1)

kruskal.test(KWH~Year)

testd2<-aov(KWH~FamNum)
sumd2<-summary(testd2)
MuHatd2 = c(mean(KWH[FamNum==2]),mean(KWH[FamNum==3]),mean(KWH[FamNum==4]));
sd2 = c(sd(KWH[FamNum==2]),sd(KWH[FamNum==3]),sd(KWH[FamNum==4]));

nd2 <- c(sum(FamNum==2),sum(FamNum==3),sum(FamNum==4));
sESTd2<-sqrt(6006947)
DFEd2 <- 117

SpSquared2 <- matrix(0,3,3);
SpSquared2[1,1] = sESTd2*sESTd2/nd2[1];
SpSquared2[2,2] = sESTd2*sESTd2/nd2[2];
SpSquared2[3,3] = sESTd2*sESTd2/nd2[3];

ad2 <- c(-1,-1,2);
Contrd2 <- t(ad2[1:3]) %*% MuHatd2;
SEContrd2 <- sqrt(t(ad2[1:3])%*%SpSquared2%*%ad2[1:3]);
tStatd2 <- Contrd2/SEContrd2;
LeftTaild2 <- pt(tStatd2,DFEd2);
RightTaild2 <- 1 - LeftTaild2;

kruskal.test(KWH~FamNum)

#part e
#two way anova
Year <- as.factor(Year);
FamNum <- as.factor(FamNum);
se1 <- c(sd(KWH[Year=="1" & FamNum=="2"]),
       sd(KWH[Year=="1" & FamNum=="3"]),
       sd(KWH[Year=="1" & FamNum=="4"]),
       sd(KWH[Year=="2" & FamNum=="2"]),
       sd(KWH[Year=="2" & FamNum=="3"]),
       sd(KWH[Year=="2" & FamNum=="4"]),
       sd(KWH[Year=="3" & FamNum=="2"]),
       sd(KWH[Year=="3" & FamNum=="3"]),
       sd(KWH[Year=="3" & FamNum=="4"]),
       sd(KWH[Year=="4" & FamNum=="2"]),
       sd(KWH[Year=="4" & FamNum=="3"]),
       sd(KWH[Year=="4" & FamNum=="4"]),
       sd(KWH[Year=="5" & FamNum=="2"]),
       sd(KWH[Year=="5" & FamNum=="3"]),
       sd(KWH[Year=="5" & FamNum=="4"]),
       sd(KWH[Year=="6" & FamNum=="2"]),
       sd(KWH[Year=="6" & FamNum=="3"]),
       sd(KWH[Year=="6" & FamNum=="4"]),
       sd(KWH[Year=="7" & FamNum=="2"]),
       sd(KWH[Year=="7" & FamNum=="3"]),
       sd(KWH[Year=="7" & FamNum=="4"]),
       sd(KWH[Year=="8" & FamNum=="2"]),
       sd(KWH[Year=="8" & FamNum=="3"]),
       sd(KWH[Year=="8" & FamNum=="4"]),
       sd(KWH[Year=="9" & FamNum=="2"]),
       sd(KWH[Year=="9" & FamNum=="3"]),
       sd(KWH[Year=="9" & FamNum=="4"]),
       sd(KWH[Year=="10" & FamNum=="2"]),
       sd(KWH[Year=="10" & FamNum=="3"]),
       sd(KWH[Year=="10" & FamNum=="4"])
       )
MuHate1 <-c(mean(KWH[Year=="1" & FamNum=="2"]),
            mean(KWH[Year=="1" & FamNum=="3"]),
            mean(KWH[Year=="1" & FamNum=="4"]),
            mean(KWH[Year=="2" & FamNum=="2"]),
            mean(KWH[Year=="2" & FamNum=="3"]),
            mean(KWH[Year=="2" & FamNum=="4"]),
            mean(KWH[Year=="3" & FamNum=="2"]),
            mean(KWH[Year=="3" & FamNum=="3"]),
            mean(KWH[Year=="3" & FamNum=="4"]),
            mean(KWH[Year=="4" & FamNum=="2"]),
            mean(KWH[Year=="4" & FamNum=="3"]),
            mean(KWH[Year=="4" & FamNum=="4"]),
            mean(KWH[Year=="5" & FamNum=="2"]),
            mean(KWH[Year=="5" & FamNum=="3"]),
            mean(KWH[Year=="5" & FamNum=="4"]),
            mean(KWH[Year=="6" & FamNum=="2"]),
            mean(KWH[Year=="6" & FamNum=="3"]),
            mean(KWH[Year=="6" & FamNum=="4"]),
            mean(KWH[Year=="7" & FamNum=="2"]),
            mean(KWH[Year=="7" & FamNum=="3"]),
            mean(KWH[Year=="7" & FamNum=="4"]),
            mean(KWH[Year=="8" & FamNum=="2"]),
            mean(KWH[Year=="8" & FamNum=="3"]),
            mean(KWH[Year=="8" & FamNum=="4"]),
            mean(KWH[Year=="9" & FamNum=="2"]),
            mean(KWH[Year=="9" & FamNum=="3"]),
            mean(KWH[Year=="9" & FamNum=="4"]),
            mean(KWH[Year=="10" & FamNum=="2"]),
            mean(KWH[Year=="10" & FamNum=="3"]),
            mean(KWH[Year=="10" & FamNum=="4"])
)
Figuree1 <- boxplot(KWH~Year+FamNum,
                   main="Boxplots of KWH by Year and Family Size",
                   xlab="Subgroups",ylab="Kilowatts");

Figuree2 <- interaction.plot(x.factor=Year,trace.factor=FamNum,
                            response=KWH,
                            fun=mean,
                            main="Interaction Plot for KWH",
                            xlab="Year",ylab="Kilowatts",
                            trace.label="Family Size"
);

teste1<-aov(KWH~Year*FamNum)
sume1<-summary(teste1)
teste2<-aov(KWH~FamNum*Year)
sume2<-summary(teste2)

#there is no non parametric test for two way anova

#part f
#logistic regression
binaryKWH=as.numeric(KWH>2601.5)
par(mfrow=c(1,2))
plot(binaryKWH~AveTemp, main="Success/Fail over Temperature", xlab="Temperature", ylab="Success/Failure")
boxplot(AveTemp~binaryKWH, main="Boxplot of Success/Fail", xlab="Success/Failure", ylab="Temperature")
logmod1=glm(binaryKWH~AveTemp, family=binomial)
summary(logmod1)
logmod2=glm(binaryKWH~AveTemp+HeatDeg+CoolDeg+Year+FamNum,family=binomial)
summary(logmod2)