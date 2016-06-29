#example of using NYT scraping functions to get and clean data
source("NYT_functions.R")
section='"Sports"'
apikey=''
sports=getMetaData(apikey = apikey, nrOfArticles = 2000, fq=makeFq(section))
length(unique(sports$urls))

#Now to real business
#sections from which we need articles
sections=c('"Sports"', '"Arts"', '"Business"', '"Obituaries"', '"World"')

#do it individually, because I've used most of my daily limit of 1000 calls
#it takes time!!!
sports=getMetaData(apikey = apikey, nrOfArticles = 3500, fq=makeFq(sections[1]))
arts=getMetaData(apikey = apikey, nrOfArticles = 3500, fq=makeFq(sections[2]))
business=getMetaData(apikey = apikey,beginDate = "20130130", 
                     nrOfArticles = 3400, fq=makeFq(sections[3]))
obituaries=getMetaData(apikey = apikey, nrOfArticles = 2400,
                       fq=makeFq(sections[4]),dayStep = 2000,
                       beginDate = "20160601")
world=getMetaData(apikey = apikey, nrOfArticles = 3200, fq=makeFq(sections[5]))

##get articles body, no scraping limit here, it takes time!!!
sports_body=getArticleBody(articleUrls = sports$urls)
sports$body=sports_body

arts_body=getArticleBody(articleUrls = arts$urls)
arts$body=arts_body

business_body=getArticleBody(articleUrls = business$urls)
business$body=business_body

obituaries_body=getArticleBody(articleUrls = obituaries$urls)
obituaries$body=obituaries_body

world_body=getArticleBody(articleUrls = world$urls)
world$body=world_body

#make list of dataframes
allDataDfList=list(sports=sports, arts=arts, business=business,
                   obituaries=obituaries, world=world)
#save data as csv files
lapply(1:length(allDataDfList), function(i)
  write.csv(allDataDfList[[i]], 
            file = paste0("./articles/",names(allDataDfList[i]), ".csv"),
            row.names = FALSE))
#save it as .RData files
lapply(names(allDataDfList), function(x) {
  x1 <- allDataDfList[[x]]
  save(x1, file=paste0("./articles/", x, '.RData'))
})

#remove duplicates and keep first 2000 observations, remove rows where
#body=NA
datadflist <- lapply(allDataDfList, function(df) {
  df=df[!duplicated(df), ]
  df=df[!is.na(df$body),]
  df=df[1:2000,]
  #add titles to text
  df$bodyTitle=paste(df$titles, df$body)
  df
})
#mark training and test set rows into data frames
datadflist <- lapply(datadflist, function(df) {
  id=sample(x=2000, size=1000)
  df$TrainTest=NA
  df$TrainTest[id]="test"
  df$TrainTest[-id]="training"
  df
})
#make doc-term matrix on train data
TrainMatrixList <- lapply(datadflist, function(df) {
  docmatrix=DocMatrixMake(df[df$TrainTest=="training",])
  list(df=docmatrix)
})
#lets also makse doc-term matrix on test data
TestMatrixList <- lapply(datadflist, function(df) {
  docmatrix=DocMatrixMake(df[df$TrainTest=="test",])
  list(df=docmatrix)
})
#calculate probabilities for training data
probMatrixList=lapply(TrainMatrixList, function(Matrix) {
  probMatrix=probabilityMatrix(as.matrix(Matrix$df))
  list(probMatrix)
})
#loop through test data to get predictions
resultsSports=c()
for(i in 1:nrow(TestMatrixList$sports$df)) {
  #get cleaned words from test doc-term matrix
  testChars<-names(data.frame(as.matrix(
    TestMatrixList$sports$df[i, as.matrix(TestMatrixList$sports$df[i,])>0])))
  testProb=sapply(probMatrixList, function(pMatrix,testChars=testChars) {
    prob= getProbability(testChars,pMatrix)
    prob
  })
  resultsSports[i]=names(testProb[testProb==max(testProb)])
  cat("Worked on article nr", i)
}
#get predicitons on individaul article categories
resultsArts=predictResult(testMatrix=TestMatrixList$arts$df,
                          probabilityMatrixList=probMatrixList) 

resultsBusiness=predictResult(testMatrix=TestMatrixList$business$df,
                          probabilityMatrixList=probMatrixList) 

resultsObituaries=predictResult(testMatrix=TestMatrixList$obituaries$df,
                              probabilityMatrixList=probMatrixList)

resultsWorld=predictResult(testMatrix=TestMatrixList$world$df,
                                probabilityMatrixList=probMatrixList)

#try to make confusn matrix-like thing to plot the results
predictedClasses=data.frame(predicted=c(resultsArts,resultsBusiness,
                                        resultsSports,
                                        resultsObituaries,resultsWorld ),
                            real=c(rep("arts",length(resultsArts)),
                                   rep("business",length(resultsBusiness)),
                                   rep("sports",length(resultsSports)),
                                   rep("obituaries",length(resultsObituaries)),
                                   rep("world",length(resultsWorld))))

finalMatrix=as.matrix(table(predictedClasses$predicted,predictedClasses$real))
#plot it as heatmap
library(pheatmap)
#colours for heatmap
YlOrBr <- c('#f7fbff','#deebf7','#c6dbef','#9ecae1','#6baed6',
            '#4292c6','#2171b5','#08519c','#08306b')
#plot it
pheatmap(finalMatrix, cluster_rows = F, cluster_cols = F, display_numbers = T,
         color=YlOrBr,fontsize=20,fontsize_number=15,number_format="%.0f",
         number_color="grey")
#accuracy
correctNrOfArticles=c()
for(i in 1:ncol(finalMatrix)) {
    correctNrOfArticles[i]=finalMatrix[i,i]
}
sum(correctNrOfArticles)/(5*1000)