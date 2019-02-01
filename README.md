# Pstat_231_Project
Final Project for PSTAT231-Data Science

s# pre prosessing
pd<-read
sb5<-subset(pd, pd$Total_Losses!="" & pd$Veh_Type_Symbol!="")
sb4<-subset(sb5, sb5$Car_Density!="" & sb5$Prior_BILimit!="")
sb1<-subset(sb4, sb4$Public_Score!="" & sb4$Credit_Score!="")
sb1$Driver_Age<-as.numeric(as.character(sb1$Driver_Age))
sb11<-subset(sb1, sb1$Driver_Age>=16 & sb1$Vehicle_Age>=-1)
sb<-na.omit(data.frame(sb11))
str(sb)
# final data subset
cost.pd2010<-subset(sb, Total_Losses>0 & Year=="2010")

###################
# start from here #
###################
load("pd2010losses.RData")
# discrete the Total_Losses #
cost.pd2010$Total_Losses<-cut(cost.pd2010$Total_Losses, 
                              breaks=c(0,1200,2700,Inf),
                              labels=c("low","medium","high"))
# training and testing sets #
set.seed(130)
train<-sample(1:nrow(cost.pd2010),2000)
pd2010.train<-cost.pd2010[train,]
pd2010.test<-cost.pd2010[-train,]

# starting regression tree
library("rpart")
# classification tree #
tree2<-rpart(Total_Losses~PD_Limit+Driver_Age+
              Driver_Point+RatedMarital+Driver_Gender+
              Credit_Score+Public_Score+Insurance_Persistency+
              Car_Density+Vehicle_Age,
             weights=Num_Claims,data=pd2010.train,
            control = rpart.control(cp=0.001))
print(tree2,cp=0.001)
printcp(tree2)
plotcp(tree2)
summary(tree2.1)
tree2.1 <- prune(tree2, cp = 0.0043)

plot(tree2)
plot(tree2.1, branch = 1, compress = F, uniform = T)
text(tree2.1, digits = 3)#, use.n = T)
# prediction discrete tree #
pred.tree1<-predict(tree2.1, newdata=pd2010.test, type="class");summary(pred.tree1)
table(pd2010.test$Total_Losses,pred.tree1)

# regression tree #
tree1<-rpart(Total_Losses~PD_Limit+Driver_Age+
               Driver_Point+RatedMarital+Driver_Gender+
               Credit_Score+Public_Score+Insurance_Persistency+
               Car_Density+Vehicle_Age,
             weights=Num_Claims,data=pd2010.train,method="anova",
             cp=0.001)
summary(tree1)
printcp(tree1)
plot(tree1)
plotcp(tree1)
text(tree1,pretty=0)
tree1.1 <- prune(tree1, cp = 0.007719)
plot(tree1.1, branch = 1, compress = F, uniform = T)
text(tree1.1, digits = 3)
plotcp(tree1.1)

pred.tree2<-predict(tree1.1,newdata=pd2010.test)
sum((pd2010.test$Total_Losses-pred.tree2)^2)

# GLM
glm1<-glm(Total_Losses~Driver_Age+
            Driver_Point+RatedMarital+Driver_Gender+
            Credit_Score+
            Vehicle_Age+
            RatedMarital*Credit_Score+Credit_Score*Vehicle_Age
          ,weights=Num_Claims,data=pd2010.train, family=Gamma(link="log"))
summary(glm1) 
# model selction #
drop1(glm1, test="Chisq")
add1(glm1, ~.^2,test="Chisq")
#search <- step(glm1, ~.^2)
#search$anova
pred.glm1<-predict.glm(glm1, newdata=pd2010.test,type="response")
sqrt(sum((pd2010.test$Total_Losses-pred.glm1)^2)/850)

# multinominal model
library(nnet)
multi.loss<-multinom(Total_Losses~Driver_Age+
                       Driver_Point+RatedMarital+Driver_Gender+
                       Credit_Score+
                       Vehicle_Age+
                       RatedMarital*Credit_Score+Credit_Score*Vehicle_Age
                     ,weights=Num_Claims,data=pd2010.train, family=Gamma(link="log"))
drop1(multi.loss,test="Chisq")
summary(multi.loss) 
pred.mult<-predict(multi.loss, newdata=pd2010.test, type="class")
table(pd2010.test$Total_Losses, pred.mult)


names(multi.loss)
multi.loss$weights
