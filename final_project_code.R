data_list=list()
for( i in 1:5){
  file_name=paste0("session",i,".rds")
  data_list[[i]]=readRDS(file_name)}

##
library(ggplot2)
num_trial_in_each_file=vector()
for(i in 1:5){
  num=length(data_list[[i]]$spks)
  num_trial_in_each_file=append(num_trial_in_each_file,num)
}

trials<-data.frame(session_name=c("Cori_12/14","Cori_12/17","Cori_12/18","Forssmann_11/01","Forssmann_11/02"),number_of_trials=num_trial_in_each_file)

knitr::kable(trials,align="cc",caption="Table 1: number of trials in each file ")

##
num_of_neurons=c()
for(i in 1:5){
  num=dim(data_list[[i]]$spks[[1]])[1]
  num_of_neurons[i]=num
}

num_of_neurons_in_each_file=data.frame(session_name=c("Cori_12/14","Cori_12/17","Cori_12/18","Forssmann_11/01","Forssmann_11/02"),number_of_neurons=num_of_neurons)

knitr::kable(num_of_neurons_in_each_file,align="cc",caption="Table 2: number of Neurons in each trial")

##
#to check na
for(i in 1:5){
  print(any(is.na(data_list[[i]])))
}

##
#transform data type
for(i in 1:5){
  data_list[[i]]$feedback_type<-as.factor(data_list[[i]]$feedback_type)
  data_list[[i]]$contrast_left<-as.factor(data_list[[i]]$contrast_left)
  data_list[[i]]$contrast_right<-as.factor(data_list[[i]]$contrast_right)
}

##
#new data set
overall_feedback<-c(data_list[[1]]$feedback_type,data_list[[2]]$feedback_typ,data_list[[3]]$feedback_typ,data_list[[4]]$feedback_typ,data_list[[5]]$feedback_typ)

overall_left_contrast<-c(data_list[[1]]$contrast_left,data_list[[2]]$contrast_left,data_list[[3]]$contrast_left,data_list[[4]]$contrast_left,data_list[[5]]$contrast_left)

overall_right_contrast<-c(data_list[[1]]$contrast_right,data_list[[2]]$contrast_right,data_list[[3]]$contrast_right,data_list[[4]]$contrast_right,data_list[[5]]$contrast_right)

##
avg_firing_rate=c()
num=1
for(i in 1:5){
  trial=length(data_list[[i]]$spks)
  for(j in 1:trial){
    neurons=dim(data_list[[i]]$spks[[j]])[1]
    avg_firing_rate[num]=sum(data_list[[i]]$spks[[j]])/neurons/0.4
    num=num+1
  }
}

##
#create new data including feedback, left contrast level, right contrast leverl, average #firing rate across each trial in five files
library(ggplot2)
#install.packages("hrbrthemes")
#install.packages("rlang")
library(rlang)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(viridis)
new_data<-data.frame(feedback=overall_feedback,contrast_left=overall_left_contrast,contrast_right=overall_right_contrast,avg_firing_rate=avg_firing_rate,session=c(rep("session 1",214),rep("session 2",251),rep("session 3",228),rep("session 4",249),rep("session 5",254)))
new_data$session<-as.factor(new_data$session)
#head(new_data)
p2 <- ggplot(data=new_data, aes(x=avg_firing_rate, group=session, fill=session)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum()
p2

##
library(pander)
#xtabs(~overall_left_contrast+overall_right_contrast,data=new_data)
new_data%>%group_by(session)%>%summarise(success_rate=paste0(round(sum(feedback==1)*100/length(feedback),2),"%"))->success
pander(success,caption="Table 3: success rate in each session",justify="center")

##
neuron_trial<-function(x,y){
  a=c()
  for(i in y){
    a=append(a,rowSums(data_list[[x]]$spks[[i]]))
  }
  return(a)
}
compare=function(x=session,y=neuron,z=trial){
  index=c(rep("last trial",y),rep("first trial",y))
  level_order=c("last trial","first trial")
  index=factor(index,level=level_order)
  newd=data.frame(index=index,firing_rate=c(neuron_trial(x,z),neuron_trial(x,1)))
  ggplot(newd, aes(x=firing_rate, color=index)) + geom_histogram(fill="white",alpha=.3,position="identity")+xlab(paste0("session",x))+ylab("neurons count")
}


first_trial=c(max(neuron_trial(1,1)),max(neuron_trial(2,1)),max(neuron_trial(3,1)),max(neuron_trial(4,1)),max(neuron_trial(5,1)))

last_trial=c(max(neuron_trial(1,214)),max(neuron_trial(2,251)),max(neuron_trial(3,228)),max(neuron_trial(4,249)),max(neuron_trial(5,254)))

session=c("session 1","session 2","session 3","session 4","session 5")

pander(data.frame(session=session,first_trial=first_trial,last_trial=last_trial),caption="Table 4 : maximum number of spikes in first trial and last trial in each session",justify="center")

##
#require(gridExtra)
#install.packages("gridExtra")
library("gridExtra")
par(mfrow=c(3,2))
plot1<-compare(1,178,214)
plot2<-compare(2,533,251)
plot3<-compare(3,228,228)
plot4<-compare(4,120,249)
plot5<-compare(5,99,254)
grid.arrange(plot1, plot2,plot3,plot4,plot5,ncol=2)

##
library(gplots)
par(mfrow=c(1,2))
plotmeans(avg_firing_rate~contrast_left,data=new_data,xlab = "left contrast levels",ylab="mean firing rate",main="figure 1: Main effect from left contrast stimulus",cex.main=0.8)
plotmeans(avg_firing_rate~contrast_right,data=new_data,xlab="right contrast levels",ylab="mean firing rate",main="figure 2: Main effect from right contrast stimulus",cex.main=0.8)

##
mean_left=c()
for(j in c("session 1","session 2","session 3","session 4","session 5")){
  for(i in c(0,0.25,0.5,1)){
    mean_left=append(mean_left,mean(new_data[new_data$contrast_left==i & new_data$session==j,4]))
  }
}

mean_right=c()
for(j in c("session 1","session 2","session 3","session 4","session 5")){
  for(i in c(0,0.25,0.5,1)){
    mean_right=append(mean_right,mean(new_data[new_data$contrast_right==i & new_data$session==j,4]))
  }
}


new_mean_left=data.frame(session=c(rep("session 1",4),rep("session 2",4),rep("session 3",4),rep("session 4",4),rep("session 5",4)),level=rep(c(0,0.25,0.5,1),5),mean_firing_rate=mean_left)

new_mean_right=data.frame(session=c(rep("session 1",4),rep("session 2",4),rep("session 3",4),rep("session 4",4),rep("session 5",4)),level=rep(c(0,0.25,0.5,1),5),mean_firing_rate=mean_right)

ggplot(new_mean_left, aes(x=as.factor(level), y=mean_firing_rate, group=session)) +
  geom_line(aes(color=session))+
  geom_point()+
  labs(title="Figure 3: mean firing rate of contrast level of left stimulus among five session")+
  theme(plot.title = element_text(size=10))


ggplot(new_mean_right, aes(x=as.factor(level), y=mean_firing_rate, group=session)) +
  geom_line(aes(color=session))+
  geom_point()+
  labs(title="Figure 4: mean firing rate of contrast level of right stimulus among five session")+theme(plot.title = element_text(size=10))

##
interaction.plot(new_data$contrast_left,new_data$contrast_right,new_data$avg_firing_rate,ylab="mean firing rate",xlab="left contrast levels",trace.label = "right contrast levels",col=c("orange","green","red","blue"),main="figure 9: interaction effect between right contrast levels and left contrast levels",cex.main=0.8)

##
par(mfrow=c(2,1))
#par(mar = c(1, 1, 1, 1))
for(i in 1:5){
  name=paste0("session ",i)
  interaction.plot(new_data[new_data$session==name,]$contrast_left,new_data[new_data$session==name,]$contrast_right,new_data[new_data$session==name,]$avg_firing_rate,ylab="mean firing rate",xlab="left contrast levels",trace.label = "right contrast levels",col=c("orange","green","red","blue"),main=paste0("figure ",i+9,": interaction effect of session",i,": right contrast levels and left contrast levels"),cex.main=0.8)
}

##
#install.packages("car")
library("lme4")
library(pander)
#install.packages("lmerTest")
library("lmerTest")
library(car)
library(carData)
options(contrasts = c("contr.treatment", "contr.poly"))
fit.full <- lmer(avg_firing_rate ~ contrast_left+contrast_right+contrast_left*contrast_right+(1|session), data=new_data)
#summary(fit_full)
pander(anova(fit.full),caption = "Table 5: Type III Analysis of Variance Table with Satterthwaite's method (continued below)",justify="center")
#proportion=(126.67/(126.67+39.95))

##
library(pander)
fit.reduced<-lmer(avg_firing_rate ~contrast_left+contrast_right+(1|session), data=new_data)
#(step_res <- step(fit.full))
#final <- get_model(step_res)
#a<-anova(final)
anova(fit.full,fit.reduced)

##
contrast_left=c()
for(i in c(0,0.25,0.5,1)){
  ans<-mean(new_data[new_data$contrast_left==i,4])-mean(new_data$avg_firing_rate)
  contrast_left<-append(contrast_left,ans)
}

contrast_right=c()
for(j in c(0,0.25,0.5,1)){
  ans<-mean(new_data[new_data$contrast_right==j,4])-mean(new_data$avg_firing_rate)
  contrast_right<-append(contrast_right,ans)
}

interaction_effect=c()
for(i in c(0,0.25,0.5,1)){
  for(j in c(0,0.25,0.5,1)){
    alpha<-mean(new_data[new_data$contrast_left==i,4])-mean(new_data$avg_firing_rate)
    beta<-mean(new_data[new_data$contrast_right==j,4])-mean(new_data$avg_firing_rate)
    ans<-mean(new_data[new_data$contrast_left==i&new_data$contrast_right==j,4])-(mean(new_data$avg_firing_rate)+alpha+beta)
    interaction_effect<-append(interaction_effect,ans)
  }
}

##
pander(data.frame(i=c(1,2,3,4),left_contrast_level=c(0,0.25,0.5,1),estimated_alpha=contrast_left),caption = "Table 6: estimated alpha_i",justify="center")
pander(data.frame(j=c(1,2,3,4),right_contrast_level=c(0,0.25,0.5,1),estimated_alpha=contrast_right),caption = "Table 7: estimated beta_j",justidy="center")
pander(data.frame(i=c(rep(1,4),rep(2,4),rep(3,4),rep(4,4)),j=rep(c(1,2,3,4),4),left_contrast_level=c(rep(0,4),rep(0.25,4),rep(0.5,4),rep(1,4)),right_contrast_level=c(0,0.25,0.5,1),estimated_alpha_beta=interaction_effect),caption = "Table 8: estimated interaction effect",justify="center")

##
levels(new_data$feedback)[levels(new_data$feedback)=="-1"]<-"0"
ans<-data.frame(table(new_data$feedback))
colnames(ans)<-c("level","count")
pander(ans,caption="Table 9: the levels of feedback and the corresponding numer of each level",justify="center")
train<-new_data[101:1196,]
test<-new_data[1:100,-1]

##
predict.full<-glm(feedback~contrast_left+contrast_right+avg_firing_rate+contrast_left*contrast_right,family=binomial,data=train)

predict.reduce<-glm(feedback~contrast_left+contrast_right+avg_firing_rate,family=binomial,data=train)
#summary(predict.reduce)
anova(predict.reduce,predict.full,test="LRT")

##
model<-glm(feedback~contrast_left+contrast_right+avg_firing_rate+contrast_left*contrast_right,family=binomial,data=train)
ans<-summary(model)
#ans
result<-data.frame(ans$coefficients[,1])
colnames(result)<-"estimated coefficient"
pander(head(result,5),caption="Table 10: first 5 estimated coeffecicient of GLM model",justify="center")

##
model_train<-glm(feedback~contrast_left+contrast_right+avg_firing_rate+contrast_left*contrast_right,family=binomial,data=train)
threshold<-0.5
predict_value<-ifelse(predict(model_train,test)>threshold,1,0)
actual_value<-new_data$feedback[1:100]
table(predict_value,actual_value)

##
pander(data.frame(Sensitivity=paste0(round(12*100/(12+62),2),"%"),Specificity=paste0(round(300/(23+3),2),"%"),Accuracy=paste0(round(65*100/(65+35),2),"%")),caption = "Table 11",justify="center")

##
#install.packages("lattice")
library(lattice)
#qqnorm(resid(fit.full))
qqmath(fit.full, id=0.05, main="figur 15: QQ-plot")#id:outlier

##
cooksD <- cooks.distance(fit.full)
influential <- cooksD[(cooksD > (3 * mean(cooksD, na.rm = TRUE)))]
#influential
infp<-which(cooksD > (3 * mean(cooksD, na.rm = TRUE)))
plot(cooksD)
points(infp,cooksD[infp],pch=17,col="red",cex=1.1)
proportion_out=length(infp)/length(cooksD)
#proportion_out

##
plot(fit.full,main="figure 16: fitted value versus residual",xlab="fitted value",ylab = "residual")

##
model<-glm(feedback~contrast_left+contrast_right+avg_firing_rate+contrast_left*contrast_right,family=binomial,data=train)
res.P = residuals(model, type = "pearson")
res.D = residuals(model, type = "deviance")
boxplot(cbind(res.P, res.D), names = c("Pearson", "Deviance"))

##
par(mfrow=c(1,2))
plot(model$fitted.values, res.P, pch=16, cex=0.6, ylab='Pearson Residuals', xlab='Fitted Values')
lines(smooth.spline(model$fitted.values, res.P, spar=0.9), col=2)
abline(h=0, lty=2, col='grey')
plot(model$fitted.values, res.D, pch=16, cex=0.6, ylab='Deviance Residuals', xlab='Fitted Values')
lines(smooth.spline(model$fitted.values, res.D, spar=0.9), col=2)
abline(h=0, lty=2, col='grey')


##
par(mfrow=c(1,1))
leverage = hatvalues(model)
plot(names(leverage), leverage, xlab="Index", type="h")
points(names(leverage), leverage, pch=16, cex=0.6)
p = length(coef(model))
n = nrow(new_data)
h=2*p/n
proportion=length(which(leverage>h))/length(leverage)
#proportion
abline(h=2*p/n,col=2,lwd=2,lty=2)

##

#install.packages("remotes")
#remotes::install_github("goodekat/redres")
#library(redres)
#launch_redres(fit.full)