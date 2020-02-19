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
    if (r[1,2]==0|r[1,3]==0)
      r[1,4] <-1
    else 
      r[1,4] <- (2*r[1,2]*r[1,3])/sum(r[1,2],r[1,3]) # Miss F
    
    r[1,5] <- xTab[2,2]/sum(xTab[,2]) # Hit Precision
    r[1,6] <- xTab[2,2]/sum(xTab[2,]) # Hit Recall
    r[1,7] <- (2*r[1,5]*r[1,6])/sum(r[1,5],r[1,6]) # Hit F
  }
  r
}


#p1<-e1[(e1$Health=='a'|e1$Health=='a')&(e1$Type==1|e1$Type==8|e1$Type==7)&(substr(e1$MTime,1,2)<" 1"|substr(e1$MTime,1,2)>"20"),]
for (mytype in 1: 5){
  if (mytype==1){
    p1<-e1[(e1$Health=='a'|e1$Health=='a')&(e1$Type==1),]
    p1$Health=as.character(p1$Health)
    p1$Health[p1$Health=='a']='O'#SA'
    
    q1<-e1[(e1$Health=='r'|e1$Health=='r')&(e1$Type==1),]
    q1$Health[q1$Health=='r']='P'#S'
    q1$Subject=as.character(q1$Subject)
    q1$Subject[q1$Subject=='isruc_g3_t']='isruc_g3_o1'
    q1$Subject[q1$Subject=='isruc_g3_65']='isruc_g3_751'
    
    p2<-f1[(f1$Health=='a'|f1$Health=='a')&(f1$Type==1),]
    p2$Health=as.character(p2$Health)
    p2$Health[p2$Health=='a']='O'#SA'
    
    q2<-f1[(f1$Health=='r'|f1$Health=='r')&(e1$Type==1),]
    q2$Health[q2$Health=='r']='P'#S'
    
  }else if (mytype==2){
    p1<-e1[(e1$Health=='a'|e1$Health=='a')&(e1$Type==1),]
    p1$Health=as.character(p1$Health)
    p1$Health[p1$Health=='a']='O'#SA'
    q1<-a1[(a1$Type==1),]
    q1$Subject=as.character(q1$Subject)
    q1$Subject[q1$Subject=='isruc_g3_2']='isruc_g3_z1'
    q1$Subject[q1$Subject=='isruc_g3_3']='isruc_g3_89'
    q1$Subject[q1$Subject=='isruc_g3_4']='isruc_g3_m'
    q1$Subject[q1$Subject=='isruc_g3_9']='isruc_g3_961'
    
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
    q1$Subject=as.character(q1$Subject)
    q1$Subject[q1$Subject=='isruc_g3_2']='isruc_g3_511'
    q1$Subject[q1$Subject=='isruc_g3_3']='isruc_g3_t1'
    q1$Subject[q1$Subject=='isruc_g3_4']='isruc_g3_471'
    q1$Subject[q1$Subject=='isruc_g3_8']='isruc_g3_821'
    
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
    
    q1$Subject=as.character(q1$Subject)
    q1$Subject[q1$Subject=='isruc_g3_2']='isruc_g3_z1'
    q1$Subject[q1$Subject=='isruc_g3_3']='isruc_g3_741'
    q1$Subject[q1$Subject=='isruc_g3_4']='isruc_g3_j1'
    q1$Subject[q1$Subject=='isruc_g3_8']='isruc_g3_821'
    
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
    q1$Subject=as.character(q1$Subject)
    q1$Subject[q1$Subject=='isruc_g3_2']='isruc_g3_z1'
    q1$Subject[q1$Subject=='isruc_g3_3']='isruc_g3_741'
    q1$Subject[q1$Subject=='isruc_g3_4']='isruc_g3_j1'
    q1$Subject[q1$Subject=='isruc_g3_8']='isruc_g3_821'
    q1$Health[q1$Health=='r']='H'#SA'
    
    p2<-f1[(f1$Health=='a'|f1$Health=='a')&(f1$Type==1),]
    p2$Health=as.character(p2$Health)
    p2$Health[p2$Health=='a']='O'#S'
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
  
  yourData<-myData
  subjects <-names(table(myData$subject))
  folds <- cut(seq(1,length(subjects)),breaks=10,labels=FALSE)
  result=array(0,c(10,4))
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
    #out1=prf(pred=pred.svm,  true =TestDatagroup)
    out1<-diag(prop.table(tab1, 1))
    if (is.nan(out1[1]))
        result[i,1]=0
      else 
        result[i,1]=out1[1]#out1[4]
    result[i,2]=out1[2]#out1[7]
    temp1=classAgreement(tab1)
    #cat(temp1$diag,"\n")
    result[i,3]=temp1$diag
    result[i,4]=length(TestDatagroup)
    #cat("\t",temp1$diag,"\t",table(TestDatagroup), "\t",mysub,"\n") 
    #Use the test and train data partitions however you desire...
  }

  cat(mytype,"\t", colMeans(result),"\n")
}
#------------------------------------

