#install.packages("randomForest")
require(randomForest)
library(e1071)
library(MASS)
Data<-read.csv("spambase.csv",header = FALSE)
names(Data)<-c('word_freq_make',"word_freq_address","word_freq_all" ,"word_freq_3d","word_freq_our","word_freq_over","word_freq_remove","word_freq_internet","word_freq_order","word_freq_mail","word_freq_receive","word_freq_will","word_freq_people","word_freq_report","word_freq_addresses","word_freq_free","word_freq_business","word_freq_email","word_freq_you","word_freq_credit","word_freq_your","word_freq_font","word_freq_000","word_freq_money","word_freq_hp","word_freq_hpl","word_freq_george","word_freq_650","word_freq_lab","word_freq_labs","word_freq_telnet","word_freq_857","word_freq_data","word_freq_415","word_freq_85","word_freq_technology","word_freq_1999","word_freq_parts","word_freq_pm","word_freq_direct","word_freq_cs","word_freq_meeting","word_freq_original","word_freq_project","word_freq_re","word_freq_edu","word_freq_table","word_freq_conference","char_freq_semicolon","char_freq_parenthesis","char_freq_bracket","char_freq_exclamation","char_freq_dollar","char_freq_hashtag","capital_run_length_average","capital_run_length_longest","capital_run_length_total" ,"spam")  
Data$spam=factor(Data$spam)
rf=c()
nb=c()
svm=c()
lda=c()
for (j in 0:3){
for (i in 1:45){
set.seed(sample(1:1000,1))
ind <- sample(2, nrow(Data), replace = TRUE, prob=c(0.6+0.1*j, 0.4-0.1*j))
Data.rf <- randomForest(spam ~ .,data=Data[ind == 1,])
Data.predrf <- predict(Data.rf, Data[ind == 2,])
Data.nb <- naiveBayes(spam ~ .,data=Data[ind == 1,])
Data.prednb <- predict(Data.nb, Data[ind == 2,])
Data.svm <- svm(spam ~ .,data=Data[ind == 1,])
Data.predsvm <- predict(Data.svm, Data[ind == 2,])
Data.lda <- lda(spam ~ .,data=Data[ind == 1,])
Data.predlda <- predict(Data.lda, Data[ind == 2,])
P <- data.frame(Data$spam[ind==2],Data.predrf,Data.prednb,Data.predsvm,Data.predlda$class )
names(P)<-c('real','rf','nb','svm','lda')
row.names(P)<- NULL
rf<-c(rf,sum(P$real==P$rf)/length(P$real))
nb<-c(nb,sum(P$real==P$nb)/length(P$real))
svm<-c(svm,sum(P$real==P$svm)/length(P$real))
lda<-c(lda,sum(P$real==P$lda)/length(P$real))
}
}
block=c()
for (i in 1:4){
for (j in 1:45){
  block=c(block,i)
}
}
Experiment<-data.frame(block,rf,nb,svm,lda)
names(Experiment)=c("Block","Random Forest", "Naive Bayes","SVM", "LDA")
#install.packages("reshape2")
library(reshape2)
Experiment=melt(data=Experiment,id.vars ="Block" ,measure.vars =c("Random Forest", "Naive Bayes","SVM", "LDA"))
names(Experiment)<-c("Block","Model","Accuracy")
write.csv(Experiment,'Experiment.csv')
