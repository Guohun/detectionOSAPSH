prf <- function(preds, trues){
  ## predAct is two col dataframe of pred,act
  #preds = tab1[,1]
  #trues = tab1[,2]
  xTab <- table(preds, trues)
  
  clss <- as.character(sort(unique(preds)))
  r <- matrix(NA, ncol = 7, nrow = 1, 
              dimnames = list(c(),c('Acc',
                                    paste("P",clss[1],sep='_'), 
                                    paste("R",clss[1],sep='_'), 
                                    paste("F",clss[1],sep='_'), 
                                    paste("P",clss[2],sep='_'), 
                                    paste("R",clss[2],sep='_'), 
                                    paste("F",clss[2],sep='_'))))
  if (ncol(xTab)==2){
    r[1,1] <- sum(xTab[1,1],xTab[2,2])/sum(xTab) # Accuracy
    r[1,2] <- xTab[1,1]/sum(xTab[,1]) # Miss Precision
    r[1,3] <- xTab[1,1]/sum(xTab[1,]) # Miss Recall
    r[1,4] <- (2*r[1,2]*r[1,3])/sum(r[1,2],r[1,3]) # Miss F
    r[1,5] <- xTab[2,2]/sum(xTab[,2]) # Hit Precision
    r[1,6] <- xTab[2,2]/sum(xTab[2,]) # Hit Recall
    r[1,7] <- (2*r[1,5]*r[1,6])/sum(r[1,5],r[1,6]) # Hit F
  }
  r
}


library(e1071)

p1<-e1[(f1$other=='d'|f1$other=='d')&(f1$Type==1|f1$Type==8),]
q1<-e1[(f1$other!='d'|f1$other=='d')&(f1$Type==1|f1$Type==8),]
#q1<-a1[(a1$Type==7|a1$Type==7),]
p2<-f1[(f1$other=='d'|f1$other=='d')&(f1$Type==1|f1$Type==8),]
q2<-f1[(f1$other!='d'|f1$other=='d')&(f1$Type==1|f1$Type==8),]
#q2<-b1[(a1$Type==7|a1$Type==7),]


loc1=which(substr(e1$MTime,1,2)>" 4"&substr(e1$MTime,1,2)<" 8")

#p1<-e1[(e1$Health=='a'|e1$Health=='a')&(e1$Type==1|e1$Type==8|e1$Type==7)&(substr(e1$MTime,1,2)<" 1"|substr(e1$MTime,1,2)>"20"),]
for (mytype in 1 : 5){
    if (mytype==1){
        p1<-e1[(e1$Health=='a'|e1$Health=='a')&(e1$Type==1),]
        p1$Health=as.character(p1$Health)
        p1$Health[p1$Health=='a']='O'#SA'
        
        q1<-e1[(e1$Health=='r'|e1$Health=='r')&(e1$Type==1),]

        p2<-f1[(f1$Health=='a'|f1$Health=='a')&(f1$Type==1),]
        p2$Health=as.character(p2$Health)
        p2$Health[p2$Health=='a']='O'#SA'
        
        q2<-f1[(f1$Health=='r'|f1$Health=='r')&(e1$Type==1),]
 
        
    }else if (mytype==2){
      p1<-e1[(e1$Health=='a'|e1$Health=='a')&(e1$Type==1),]
      p1$Health=as.character(p1$Health)
      p1$Health[p1$Health=='a']='O'#SA'
      q1<-a1[(a1$Type==1),]
      p2<-f1[(f1$Health=='a'|f1$Health=='a')&(f1$Type==1),]
      p2$Health=as.character(p2$Health)
      p2$Health[p2$Health=='a']='O'#SA'
      #p2$Health[p2$Health=='r']='P'#S'
      q2<-b1[(b1$Type==1),]
      
    }else if (mytype==3){
      p1<-e1[(e1$Health=='r'|e1$Health=='r')&(e1$Type==1),]
      p1$Health=as.character(p1$Health)
      p1$Health[p1$Health=='r']='P'#SA'
      q1<-a1[(a1$Type==1),]
      p2<-f1[(f1$Health=='r'|f1$Health=='r')&(f1$Type==1),]
      p2$Health=as.character(p2$Health)
      p2$Health[p2$Health=='r']='P'#S'
      q2<-b1[(b1$Type==1),]
      
    }else if (mytype==4){
      p1<-e1[(e1$Health=='a'|e1$Health=='r')&(e1$Type==1),]
      p1$Health=as.character(p1$Health)
      p1$Health[p1$Health=='a']='P'#SA'
      p1$Health[p1$Health=='r']='P'#SA'
      q1<-a1[(a1$Type==1),]
      p2<-f1[(f1$Health=='a'|f1$Health=='r')&(f1$Type==1),]
      p2$Health=as.character(p2$Health)
      p2$Health[p2$Health=='a']='P'#S'
      p2$Health[p2$Health=='r']='P'#S'
      q2<-b1[(b1$Type==1),]
    }else if (mytype==5){
      p1<-e1[(e1$Health=='a'|e1$Health=='a')&(e1$Type==1),]
      p1$Health=as.character(p1$Health)
      p1$Health[p1$Health=='a']='O'#SA'
      
      q1<-rbind( a1[(a1$Type==1),1:20],e1[(e1$Health=='r'|e1$Health=='r')&(e1$Type==1),1:20])
      q1$Health[q1$Health=='r']='H'#SA'
      
      p2<-f1[(f1$Health=='a'|f1$Health=='a')&(f1$Type==1),]
      p2$Health=as.character(p2$Health)
      p2$Health[p2$Health=='a']='O'#S'
      #p2$Health[p2$Health=='r']='P'#S'
      #q2<-b1[(b1$Type==1),]
      q2<-rbind(b1[(b1$Type==1),1:20],f1[(f1$Health=='r'|f1$Health=='r')&(f1$Type==1),1:20])
    }

      myData <- data.frame(MD_c3=c(p1$MD,q1$MD),
                           MD_c4=c(p2$MD,q2$MD),
                           #HC3=c(p1$MaxD,q1$MaxD),
                           #                     HC4=c(p2$MaxD*p1$MaxD,q2$MaxD*q1$MaxD), 
                           GE_c3=c(p1$CC,q1$CC),
                           GE_c4=c(p2$CC,q2$CC),
                           NL_c3=c(p1$L1*20,q1$L1*20),  #20  6000
                           NL_c4=c(p2$L1*20,q2$L1*20),#subject=c(p1$Subject,q1$Subject),
                           #C33=c(p1$DFS,q1$DFS),
                           NS_c3=c(p1$DFS/300,q1$DFS/300),
                           NS_c4=c(p2$DFS/300,q2$DFS/300),
                           #                     L5_L=c(p1$L9,q1$L9),
                           #                     L5_R=c(p2$L9,q2$L9),
                           #                     L54=c(p2$L10*p1$L10,q2$L10*q1$L10),
                           subject=c(as.character(p1$Subject),as.character(q1$Subject)),
                           #                     Type=c(p1$Type,q1$Type),                     
                           #                     group=as.factor(c(rep(1,nrow(p1)),rep(2,nrow(q1)))))
                           group=c(as.character(p1$Health),rep('H',nrow(q1))))
      
      #myData=
      yourData<-myData[,-9] #[sample(nrow(myData)),]
      folds <- cut(seq(1,nrow(yourData)),breaks=10,labels=FALSE)
      library(caret)
      result=array(0,c(10,6))
      for(i in 1:10){
        #Segement your data by fold using the which() function 
        testIndexes <- which(folds==i,arr.ind=TRUE)
        split=0.2
        testIndexes <- createDataPartition(yourData$group, p=split, list=FALSE)      
        testData <- yourData[testIndexes, -9]
        trainData <- yourData[-testIndexes, ]
        TestDatagroup<-as.character(yourData$group[testIndexes])
#        evenvals <- seq(1, nrow(yourData), by=2)
 #       trainData <-yourData[evenvals,]
#        testData <-yourData[-evenvals,-9]
#        TestDatagroup<-yourData$group[-evenvals]
        
        svmfit=svm(factor(group) ~ ., data = trainData, scale = T, kernel = "radial", gamma=0.80, cost = 20)
        #print(svmfit)
        #plot(svmfit, myData, mean_CC~mean_DFS)
        pred.svm = predict(svmfit, testData)
        tab1=table(pred=pred.svm,  true =TestDatagroup)
        #cat(diag(prop.table(tab1, 1)),"\t")
        out1<-diag(prop.table(tab1, 1))
        result[i,1]=out1[1]
        result[i,2]=out1[2]
        out1=prf(pred=pred.svm,  true =TestDatagroup)
        result[i,3]=out1[4]
        result[i,4]=out1[7]
        
        temp1=classAgreement(tab1)
        #cat("\t",temp1$diag,"\t",unique(TestDatagroup), "\t",unique(myData$subject[testIndexes]), "\n") 
        result[i,5]=temp1$diag
        result[i,6]=length(TestDatagroup)
      }
      cat(colMeans(result),"\n")
}
#------------------------------------

healthy_dt <-myData

health1=ddply(healthy_dt,~group,summarise,MD_c3=mean(MD_c3),DFS=mean( myData$NS_c3),
              mean_MaxD=mean(myData$GE_c3),mean_L0=mean(myData$NL_c3),
              mean_L1=mean(myData$GE_c4),mean_L2=mean(myData$NS_c4),mean_L3=mean(L3),
              mean_L4=mean(L4),mean_CC=mean(CC),mean_age=mean(age)
              #,sd_MD=sd(MD),sd_CC=sd(CC),sd_L0=sd(L0)
)



#--------------------------------

#Randomly shuffle the data
myData=myData[,-9]
yourData<-myData[sample(nrow(myData)),]
#Create 10 equally size folds
folds <- cut(seq(1,nrow(yourData)),breaks=10,labels=FALSE)
result=c(0,array(0,9))
#Perform 10 fold cross validation
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- yourData[testIndexes, -9]
  trainData <- yourData[-testIndexes, ]
  TestDatagroup<-as.character(yourData$group[testIndexes])
  
  svmfit=svm(factor(group) ~ ., data = trainData, scale = T, kernel = "radial", gamma=0.80, cost = 20)
  #print(svmfit)
  #plot(svmfit, myData, mean_CC~mean_DFS)
  pred.svm = predict(svmfit, testData)
  tab1=table(pred=pred.svm,  true =TestDatagroup)
  diag(prop.table(tab1, 1))
  temp1=classAgreement(tab1)
  cat(temp1$diag,"\n")
  result[i]=temp1$diag
}


yourData<-myData
#Create 10 equally size folds
subjects <-names(table(myData$subject))
#Perform 10 fold cross validation
for(sub in subjects){
  #Segement your data by fold using the which() function 
  yourData <-myData[,-9]
  testIndexes <- which(myData$subject==sub)
  trainData <- yourData[-testIndexes, ]
  testData <- yourData[testIndexes, -9]
  
  TestDatagroup<-as.character(yourData$group[testIndexes])
  svmfit=svm(factor(group) ~ ., data = trainData, scale = T, kernel = "radial", gamma=0.80, cost = 20)
  #print(svmfit)
  #plot(svmfit, myData, mean_CC~mean_DFS)
  pred.svm = predict(svmfit, testData)
  tab1=table(pred=pred.svm,  true =TestDatagroup)
  diag(prop.table(tab1, 1))
  temp1=classAgreement(tab1)
  cat(sub,"\t",temp1$diag,"\t",length(TestDatagroup), "\n") 
  #Use the test and train data partitions however you desire...
}
#--------------------------------------------




yourData<-myData
subjects <-names(table(myData$subject))
folds <- cut(seq(1,length(subjects)),breaks=10,labels=FALSE)
result=array(0,c(9,3))
for(i in 1:10 ){
  yourData <-myData[,-9]
  mysub <- subjects[which(folds==i,arr.ind=TRUE)]
  testIndexes <- which(myData$subject %in%  mysub)
  trainData <- yourData[-testIndexes, ]
  testData <- yourData[testIndexes, -9]
  
  TestDatagroup<-as.character(yourData$group[testIndexes])
  svmfit=svm(factor(group) ~ ., data = trainData, scale = T, kernel = "radial", gamma=0.80, cost = 20)
  #print(svmfit)
  #plot(svmfit, myData, mean_CC~mean_DFS)
  pred.svm = predict(svmfit, testData)
  tab1=table(pred=pred.svm,  true =TestDatagroup)
  #diag(prop.table(tab1, 1))
  out1=prf(pred=pred.svm,  true =TestDatagroup)
  #out1<-diag(prop.table(tab1, 1))
  result[i,1]=out1[4]
  result[i,2]=out1[7]
  temp1=classAgreement(tab1)
  #cat(temp1$diag,"\n")
  result[i,3]=temp1$diag
  cat(sub,"\t",temp1$diag,"\t",length(TestDatagroup), "\n") 
  #Use the test and train data partitions however you desire...
}

library(dplyr) # for data manipulation
library(caret) # for model-building
library(DMwR) # for smote implementation
library(purrr) # for functional programming (map)
library(pROC) # for AUC calculations

library(dplyr)


un_male=e1[(e1$Health=='r'|e1$Health=='a')&(e1$Type==1|e1$Type==7|e1$Type==8)&(substr(e1$MTime,1,2)<" 1"|substr(e1$MTime,1,2)>"20"),  c(1,3,5,6,7,8,9,10,11,12:150,4,169)]
un_male_f=f1[(f1$Health=='r'|f1$Health=='a')&(f1$Type==1|f1$Type==7|f1$Type==8)&(substr(e1$MTime,1,2)<" 1"|substr(e1$MTime,1,2)>"20"),c(1,3,5,6,7,8,9,10,11,12:150,4,169)]

un_male=e1[(e1$Health=='r'|e1$Health=='a')&(e1$Type==1),  c(1,3,5,6,7,8,9,10,12,13:18)]
un_male_f=f1[(f1$Health=='r'|f1$Health=='a')&(f1$Type==1),c(1,3,5,6,7,8,9,10,12,13:18)]

un_male$Health <- factor(un_male$Health)


df_e <- un_male %>%group_by(Subject, Health) %>%summarise_each(funs(mean))
df_f <- un_male_f %>%group_by(Subject, Health) %>%summarise_each(funs(mean))
max_e <- un_male %>%group_by(Subject, Health) %>% summarise(MD=min(MD))
max_f <- un_male_f %>%group_by(Subject, Health) %>% summarise(MD=min(MD))

max_eCC <- un_male %>%group_by(Subject, Health) %>% summarise(MD=min(CC))
max_fCC <- un_male_f %>%group_by(Subject, Health) %>% summarise(MD=min(CC))
max_eDFS <- un_male %>%group_by(Subject, Health) %>% summarise(MD=min(DFS))
max_fDFS <- un_male_f %>%group_by(Subject, Health) %>% summarise(MD=min(DFS))
max_eL0 <- un_male %>%group_by(Subject, Health) %>% summarise(MD=max(L0))
max_fL0 <- un_male_f %>%group_by(Subject, Health) %>% summarise(MD=max(L0))

  
#df_g <- un_male %>%group_by(Subject, Gender) %>%summarise_each(funs(mean))

apnea1=data.frame(group=max_f$Health,
                  #MD_c3=df_e$MD,MD_c4=df_f$MD,
                  #GE_c3=df_e$CC,GE_c4=df_f$CC,
                  #NS_c3=df_e$DFS,NS_c4=df_f$DFS,
                  #NL_c3=df_e$L0, NL_c4=df_f$L0
                  #NL_c3=df_e$MaxD, NL_c4=df_f$MaxD
                  mMD_c3=max_e$MD,mMD_c4=max_f$MD,
                  mGE_c3=max_eCC$MD,mGE_c4=max_fCC$MD,
                  mNS_c3=max_eDFS$MD/300,mNS_c4=max_fDFS$MD/300,
                  mNL_c3=max_eL0$MD,mNL_c4=max_fL0$MD
)

apnea=apnea1



evenvals <- seq(1, nrow(apnea), by=2)
myData <-apnea[evenvals,]
TestData <-apnea[-evenvals,-1]
TestDatagroup<-apnea$group[-evenvals]

#svmfit=svm(factor(group) ~ ., data = myData, scale = T, kernel = "radial", cost = 20,gamma=0.80)
svmfit=svm(factor(group) ~ ., data = myData, scale = T, kernel = "radial", cost = 20,gamma=0.78)
#print(svmfit)
#plot(svmfit, apnea,mean_CC~mean_MD)
pred.svm = predict(svmfit, TestData)
tab1=table(pred=pred.svm,  true =TestDatagroup)
out1=prf(pred=pred.svm,  true =TestDatagroup)
diag(prop.table(tab1, 1))
temp1=classAgreement(tab1)
