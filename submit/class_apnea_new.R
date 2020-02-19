


library(e1071)

TestFile="asymetric/isruc_un_1_35_hvg_nop_noc_ch4.txt"
e0<- read.delim(file=TestFile, header=T, sep="\t")
TestFile="asymetric/srucg3_35_70_dvg_hvg_nop_noc_ch4.txt"
e2<- read.delim(file=TestFile, header=T, sep="\t")
TestFile="asymetric/isruc_un_71_100_hvg_nop_noc_ch4.txt"
e3<- read.delim(file=TestFile, header=T, sep="\t")
e1<-rbind(e0,e2,e3)

e1=e1[e1$Type<9,]
#e1$Type[e1$Type>=7]=1
e1$Type[e1$Type==6]=5

TestFile="asymetric/isruc_un_1_35_hvg_nop_noc_ch7.txt"
f0<- read.delim(file=TestFile, header=T, sep="\t")
TestFile="asymetric/srucg3_35_70_dvg_hvg_nop_noc_ch7.txt"
f2<- read.delim(file=TestFile, header=T, sep="\t")
TestFile="asymetric/isruc_un_71_100_hvg_nop_noc_ch4.txt"
f3<- read.delim(file=TestFile, header=T, sep="\t")
f1<-rbind(f0,f2,f3)
f1=f1[f1$Type<9,]
#f1$Type[f1$Type>=7]=1
f1$Type[f1$Type==6]=5


e1$Health[e1$Subject=='isruc_g3_d']='a'
f1$Health[f1$Subject=='isruc_g3_d']='a'


TestFile="asymetric/srucg1_1_10_dvg_hvg_nop_noc_ch4.txt"
a1<- read.delim(file=TestFile, header=T, sep="\t")

a1=a1[a1$Type<9,]
#a1$Type[a1$Type>=7]=1
a1$Type[a1$Type==6]=5

TestFile="asymetric/srucg1_1_10_dvg_hvg_nop_noc_ch7.txt"
b1<- read.delim(file=TestFile, header=T, sep="\t")

b1=b1[b1$Type<9,]
#b1$Type[b1$Type>=7]=1
b1$Type[b1$Type==6]=5



p1<-e1[(e1$Health=='a'|e1$Health=='r')&(e1$Type==1),]
p1$Health=as.character(p1$Health)
p1$Health[p1$Health=='a']='O'#SA'
p1$Health[p1$Health=='r']='P'#S'

q1<-a1[(a1$Type==1),]


p2<-f1[(f1$Health=='a'|f1$Health=='r')&(f1$Type==1),]
p2$Health=as.character(p2$Health)
p2$Health[p2$Health=='a']='O'#SA'
p2$Health[p2$Health=='r']='P'#S'
q2<-b1[(b1$Type==1),]

myData <- data.frame(MD_c3=c(p1$MD,q1$MD),
                     MD_c4=c(p2$MD,q2$MD),
                     #HC3=c(p1$MaxD,q1$MaxD),
                     GE_c3=c(p1$CC,q1$CC),
                     GE_c4=c(p2$CC,q2$CC),
                     NL_c3=c(p1$L1*20,q1$L1*20),  #20  6000
                     NL_c4=c(p2$L1*20,q2$L1*20),#subject=c(p1$Subject,q1$Subject),
                     #C33=c(p1$DFS,q1$DFS),
                     NS_c3=c(p1$DFS/300,q1$DFS/300),
                     NS_c4=c(p2$DFS/300,q2$DFS/300),
                     subject=c(as.character(p1$Subject),as.character(q1$Subject)),
                     group=c(as.character(p1$Health),rep('H',nrow(q1))))


write.csv(myData, file = "SleepData.csv")


library(circular)
aov.circular(myData$MD_c3, myData$group)


temp=myData[,-9]

means.long<-melt(temp[temp$MD_c3<8&temp$MD_c4<8,],id.vars="group")

colorder <- c( "green", "red", "blue")
p <- ggplot(data = means.long, aes(x=variable, y=value)) + 
    geom_boxplot(aes(fill=group))


p+facet_wrap(~variable, scales = "free", nrow = 4, 
           strip.position = "left", 
           labeller = as_labeller(c(MD_c3 = "Degree", MD_c4 = "Degree",
                                    GE_c3 = "Entropy", GE_c4 = "Entropy",
                                    NL_c3 = "Isolated value", NL_c4 = "Isolated value",
                                    NS_c3 = "Components", NS_c4 = "Components") )) + 
        ylab(NULL) +
        xlab(NULL) +
        scale_fill_discrete(breaks=c("H","O","P"),labels=c("Health","OSA","PS")) +
        scale_x_discrete(labels =c("Mean degree of C3","Mean degree of C4",
                                   "Graph entropy of C3","Graph entropy of C4",
                                   "Leaf nodes of C3","Leaf nodes of C4",
                                   "Triangles of C3","Triangles of C4"))
xloc=which(myData$group=='O')
wilcox.test(myData$MD_c3[xloc],myData$MD_c4[xloc], alternative = "less")
shapiro.test(myData$MD_c3[xloc])
var.test(myData$MD_c3[xloc],myData$MD_c4[xloc])
wilcox.test(myData$MD_c3[xloc],myData$MD_c4[xloc], var.equal = F)


#-------------------------Classification by 50/50 (odd, even)-------------------
myData <-myData[,-9]
evenvals <- seq(1, nrow(myData), by=2)
myData1 <-myData[evenvals,]
TestData <-myData[-evenvals,-9]
TestDatagroup<-myData$group[-evenvals]

svmfit=svm(factor(group) ~ ., data = myData1, scale = T, kernel = "radial", gamma=0.80, cost = 20)
#print(svmfit)
#plot(svmfit, myData, mean_CC~mean_DFS)
pred.svm = predict(svmfit, TestData)
tab1=table(pred=pred.svm,  true =TestDatagroup)
out1=prf(pred=pred.svm,  true =TestDatagroup)
diag(prop.table(tab1, 1))
temp1=classAgreement(tab1)

#Calculating precision
tab1[1, 1]/(tab1[1, 1] +tab1[1, 2])
#Calculate accuracy
sum(diag(tab1))/sum(sum(tab1))

prf(pred=pred.svm,  true =TestDatagroup)


#-------------------------summary epochs on each subject---

table(e1$Subject[e1$Type==1|e1$Type==8|e1$Type==7])

#-------------------------------------------------------------
