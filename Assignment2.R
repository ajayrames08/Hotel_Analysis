x1=read.csv("text_123.csv",stringsAsFactors = F)
names(x1)
x1$hotel.name = as.factor(x1$hotel.name)
str(x1)
library(tm)
corh=Corpus(VectorSource(c(x1$rev.title)))
coro=Corpus(VectorSource(c(x1$rev)))

corh=tm_map(corh,tolower)
corh=tm_map(corh,PlainTextDocument)
corh=tm_map(corh,removePunctuation)
#corh=tm_map(corh,removeWords,c("told",stopwords("english")))
#corh=tm_map(corh,removeWords,stopwords("english"))
corh=tm_map(corh,stemDocument)

coro=tm_map(coro,tolower)
coro=tm_map(coro,PlainTextDocument)
coro=tm_map(coro,removePunctuation)
#coro=tm_map(coro,removeWords,c("told",stopwords("english")))
coro=tm_map(coro,removeWords,c("bit","get","Get","said","told","and","sands","sand","singapore","Said"))
coro=tm_map(coro,stemDocument)

dtm1=DocumentTermMatrix(corh)
dtm2=DocumentTermMatrix(coro)
dtm2
inspect(dtm2[1000:1005,505:515])

# Check for sparsity

findFreqTerms(dtm2, lowfreq=5)

sparse1=removeSparseTerms(dtm1,0.99)
sparse2=removeSparseTerms(dtm2,0.998)

sparse1
sparse2

x2=as.data.frame(as.matrix(sparse1))
x3=as.data.frame(as.matrix(sparse2))

colnames(x2)=make.names(colnames(x2))
colnames(x3)=make.names(colnames(x3))

View(x2)


colnames(x2)=paste0("H",colnames(x2))
colnames(x3)=paste0("R",colnames(x3))

#x2$rt.overall=x1$rt.ovarll
#x3$rt.overall = x2$rt.ovarll

x =cbind(x2,x3)

names(x1)
#x$wordcount=x1$Word.count
#x$wordcount = NULL
#x$rt.overall=x1$rt.ovarll
head(x3$said)

x3$rt.overall = x1$rt.ovarll
#names(x)
library(caTools)
set.seed(1000)

spl=sample.split(x3$rt.overall,SplitRatio = 0.7)
train=subset(x3,spl==T)
test=subset(x3,spl==F)

library(rpart)
library(rpart.plot)
set.seed(1000)
model1=rpart(rt.overall~.,data=train,method="class")
prp(model1)

test1=test

View(train)

test1$wordcount=NULL
predict1=predict(model1,newdata=test, type="prob")

table(test$rt.overall, predict1)

head(test$rt.overall)

head(predict1)
names(predict1)
View(predict1)

######################################################################################

head(x1$)

full = subset(x1,x1$hotel.name=="Fullerton")


