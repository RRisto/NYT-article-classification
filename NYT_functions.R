########################################################################
#################functions for scraping metadata and articles
#helper function to make NYT API call url
makeURL <- function(q=NULL, fq=NULL, begin_date=NULL, end_date=NULL, 
                    key=apikey, page=0, sort=NULL, fl=NULL, 
                    hl=NULL, facet_field=NULL,facet_filter=NULL){
  #input= argumenst of NYT API call, more about arguments read here: 
  #https://developer.nytimes.com/article_search_v2.json#/README
  #output (character string)= url for NYT API call
  arglist <- list(q=q, fq=fq, begin_date=begin_date, end_date=end_date, 
                  'api-key'=key, page=page, sort=sort, fl=fl, hl=hl,
                  facet_field=facet_field,
                  facet_filter=facet_filter)
  url <- paste0('http://api.nytimes.com/svc/search/v2/articlesearch.json?')
  for(i in 1:length(arglist)){
    if(is.null(unlist(arglist[i]))==F){
      url <- paste0(url, '&', names(arglist[i]), '=', arglist[i])
    }
  }
  return(url)
}

#small helper function to make section names for query 
makeFq=function(section){
  #https://developer.nytimes.com/article_search_v2.json#/README
  #input (character)= section name(s) (double qoutation!): '"Sports", "Arts"'
  #output (character)= fq variable for getMetaData function.
    paste0('section_name:(', section,')')
}

#function that gets meta data of specified number of articles
#takes them as as dayStep specified chunks 
#(max 1000 articles per day for given filters)
getMetaData=function(apikey,nrOfArticles=300, beginDate="20160619", 
                     backward=T, sort=NULL,fq =NULL, fl=NULL, 
                     hl=NULL, facet_field=NULL, dayStep=1,
                     facet_filter=NULL) {
  #input:
  #apikey (character)=your key for NYT API.
  #nrOfArticles (integer)=nr of articles which metadata you want to scrape. 
  #beginDate (character, format: YYYYMMDD)=date from which the scraping begins. 
  #backward (boolean)=how scraping is done considering time scale: from begin 
  #date to future or to the past, deafult to the past. 
  #section (character, use function makeFq)=section which articles you want 
  #to scrape rest of the arguments are from function makeURL
  #dayStep (integer) = from how big period chunks articles will be taken.
  #by default will take daily, if articles are widely distributed in time 
  #it is reasonable to take bigger chunks
  #output: dataframe with following columns: urls, section_names, titles,
  #dates
  require(jsonlite)
  library(RCurl)
  #initial sanity check, if there are some articles in sepcified nr of loops,
  #and asks user what to do
  beginDateinside=beginDate
  endDateinside=beginDate
  if(backward==T) {
    #date from which begin (max period based on daystep and API call limit)
    checkDate=gsub("-","",as.Date(strptime(beginDateinside, 
                                           "%Y%m%d"))-(999*dayStep))
    checkurl=makeURL(fq =fq,begin_date=checkDate, end_date=endDateinside,
                             sort=sort, fl=fl, hl=hl, facet_field=facet_field,
                             facet_filter=facet_filter)
  } else {
    checkDate=gsub("-","",as.Date(strptime(beginDateinside, 
                                           "%Y%m%d"))+(999*dayStep))
    checkurl=makeURL(fq =fq,begin_date=beginDateinside, end_date=checkDate,
                             sort=sort, fl=fl, hl=hl, facet_field=facet_field,
                             facet_filter=facet_filter)
  }
  #nr of articles for this period
  hits=fromJSON(txt=checkurl, flatten = T)$response$meta$hits
  #if hits is smaller than nrOfAticles, than stop
  if(hits<nrOfArticles) {
    stop(paste0("There are ", hits," articles, but you wanted ",nrOfArticles, 
               ". Please specify nrOfArticles/beginData/dayStep"))
  }
  # set up initial user choiche which is False
  userChoice="I have no idea"
  #ask user choice
  while (!userChoice%in%c("1", "0")) {
    userChoice=readline(prompt=paste0(hits, " hits from ", checkDate, " to ", 
                                      beginDate,". To continue press 1,
                                      to stop press 0: "))
  }
  #act according to user choice
   if (userChoice=="0") {
    stop("Stopping this session") #stop session
  } else if (userChoice=="1") {
    cat("Let's rock and roll \n") #continue
  }
  
#######function main body
  #initialize variables to where we loop information needed
  urls=c()
  section_names=c()
  titles=c()
  dates=c()
  callCounter=1#counts nr of all calls to avoid infinite loops and API limit
  #start looping, second condition is stupid way to escape infinite loop
  while(length(urls)<nrOfArticles&&callCounter<=999) {
    #initialise dates between which we search for articles
    if(backward==T) {
      endDateinside=gsub("-","",
                         as.Date(strptime(beginDateinside, "%Y%m%d"))-1)
      beginDateinside=gsub("-","",
                           as.Date(strptime(beginDateinside, "%Y%m%d"))-
                             dayStep)
    } else {
      beginDateinside=gsub("-","",as.Date(strptime(endDateinside, "%Y%m%d"))+
                             1)
      endDateinside=gsub("-","",as.Date(strptime(endDateinside, "%Y%m%d"))+
                           dayStep)
      }
    #when error occurs display message and continue
    tryget <- try({
      #initial nr of hits
      hits=0
      callTry=1#nr of tries to display the number for user
      #loop until find page that has non-0 hits
      #second condition is stupid way to escape infinite loop
       while(hits==0&&callCounter<=999) {
        initcall=getURL(makeURL(fq =fq,begin_date=beginDateinside, 
                                end_date=endDateinside,
                                sort=sort, fl=fl, hl=hl, 
                                facet_field=facet_field,
                                facet_filter=facet_filter))
        #nr of articles in query
        hits=fromJSON(txt=initcall, flatten = T)$response$meta$hits
        callCounter=callCounter+1
        cat("Looking for calls that have some hits. Try nr ", 
            callTry, " \n")
        callTry=callTry+1
     }
    #nr of loops needed for that query page where there are at leats 1 hits
      #(max 99, because max 100 pages (starting from 0!) are allowed
      #by NYT API per unique url
    nloops=min(max(ceiling(hits/10-1),0), 99)
    #loop meta data from pages
    for(i in 0:nloops) {#start looping from 0 because page nr start from 0
      if(length(urls)<nrOfArticles) {
                url=makeURL(fq =fq,page=i,sort=sort, fl=fl, hl=hl,
                            facet_field=facet_field,
                            facet_filter=facet_filter,
                            begin_date=beginDateinside, 
                            end_date=endDateinside)
                response=fromJSON(txt=url, flatten = T)
        #append data into our vectors        
        urls=append(urls, response$response$docs$web_url)
        section_names=append(section_names,response$response$docs$section_name)
        titles=append(titles, response$response$docs$headline.main)
        dates=append(dates,response$response$docs$pub_date )
        #display message for user
        cat("Metadata for", length(urls), "articles. API call nr ",
            callCounter,  " \n")
        #prints also link, needed for debugging
        print(makeURL(fq =fq,page=i,sort=sort, fl=fl, hl=hl, 
                      facet_field=facet_field,facet_filter=facet_filter,
                      begin_date=beginDateinside, end_date=endDateinside))
        callCounter=callCounter+1
        Sys.sleep(0.2) #not ot make too many calls (5 per second are allowed)
      } else {
        #maybe I can delete this
        results=data.frame(urls, section_names, titles,dates)
      }
    }
    }) #end of tryget
    #if some error happened, give message with url and continue
    if(class(tryget)=='try-error') { 
        cat("page number:",length(urls)+1, ' error - body not scraped, url:',
            makeURL(fq =fq,page=i,sort=sort, fl=fl, hl=hl, 
                    facet_field=facet_field,facet_filter=facet_filter,
                    begin_date=beginDateinside, end_date=endDateinside),'\n')
      #write NAs to vairables we are scraping
        urls[i]=NA
        section_names[i]=NA
        titles[i]=NA
        dates[i]=NA
        next
    }
    callCounter=callCounter+1
  }
  results=data.frame(urls, section_names, titles,dates)
  #display results  
  results
}

#function to get article body from metadata scraped
getArticleBody=function(articleUrls, 
                        selector=c('article > div', '.articleBody')) {
  #input: 
  #articleUrls (character vector): article urls from metadata via NYT API 
  #selector (character vector)= selectors which indicate articles body, might
  #be more, needs to be tested to find out
  #output (character vector)=article bodys
  library(rvest)
  body=c()#initialize vector where we add article body
  for (i in 1:length(articleUrls)) {
    url=as.character(articleUrls[i])
    
    tryget <- try({ #is needed if some error happens, then it continues
    page=read_html(url)
    for (j in 1:length(selector)) {
      if(length(page %>% #if response has 0 characters of body
                html_nodes(selector[j]) %>%
                html_text())==0) {
        body[i]=NA
      } else {
        body[i]=paste(page %>% #paste needed to collapse char vec into 1 vec
                        html_nodes(selector[j]) %>%
                        html_text(), collapse = '')
        break #if body found no need to try other selectors
      }
    }
    cat("Worked on article nr",paste0(i, ","),
        paste0("progress: ",round(i/length(articleUrls)*100,3),"%"),"\n")
      })
    #if some error occured, give message and continue
       if(class(tryget)=='try-error') { 
        cat("page number:",i, ' error - body not scraped \n')
         body[i]=NA
         next
    }
  }
  body
}

##############################################################
####################function to clean and preprocess data for modelling
DocMatrixMake=function(dataframe) {
  #input:
  #dataframe=dataframe which contains body and titles of articles, 
  #column name which has article body and title must be bodyTitle
  #output:
  #document-term matrix (matrix)=columns as words and rows as documents
  
  #add title to body text, because it is needed for analysis
  dataframe$bodyTitle=gsub("[^A-Za-z]"," ", dataframe$bodyTitle)
  library(tm)
  #create corpus, and make word remove numbers, stemming and stop words
  data.corpus <- Corpus(VectorSource(as.vector(dataframe$bodyTitle)))
  data.corpus= tm_map(data.corpus, stripWhitespace)
  data.corpus=tm_map(data.corpus, content_transformer(tolower))
  data.corpus=tm_map(data.corpus, removeWords, stopwords("english"))
  data.corpus=tm_map(data.corpus, stemDocument)
  t(TermDocumentMatrix(data.corpus))#transpose it
}

####################################################################
############functions for model making

#function for probability matrix, calculates each word probability 
#being in specific article class
probabilityMatrix <-function(docMatrix)
  #input:
  #document term matrix from function DocMatrixMake
  #output:
  #matrix of each term count, additive sum, probability, lnProbability
{
  # Sum up the term frequencies
  termSums<-cbind(colnames(as.matrix(docMatrix)),
                  as.numeric(colSums(as.matrix(docMatrix))))
  # Add one
  termSums<-cbind(termSums,as.numeric(termSums[,2])+1)
  # Calculate the probabilties
  termSums<-cbind(termSums,(as.numeric(termSums[,3])/
                              sum(as.numeric(termSums[,3]))))
  # Calculate the natural log of the probabilities
  termSums<-cbind(termSums,log(as.numeric(termSums[,4])))
  # Add pretty names to the columns
  colnames(termSums)<-c("term","count","additive","probability","lnProbability")
  termSums
}

#function to get probabilities for test text
getProbability <- function(testChars,probabilityMatrix)
  #input:
  #testChars (character vector)=characters on test set article from which
  #we must predict class. Must be cleaned before using DocMatrixMake()
  #probabilityMatrix (matrix)=probabilities of words based on training data
  #using function probabilityMatrix()
  #output:
  #prob (numeric vector)=vector of probability for each class 
  
{
  #turn probMatrix into data frame
  probabilityMatrix=data.frame(probabilityMatrix)
  #caharacters found in test data
  charactersFound<-probabilityMatrix[probabilityMatrix[,1] %in% testChars,
                                     "term"]
  # Count how many words were not found in the traininig matrix
  charactersNotFound<-length(testChars)-length(charactersFound)
  # Add the normalized probabilities for the words founds together
  charactersFoundSum<-sum(as.numeric(as.character(probabilityMatrix[probabilityMatrix[,1] %in% testChars,"lnProbability"])))
  # We use ln(1/total smoothed words) for words not found
  charactersNotFoundSum<-charactersNotFound*log(1/sum(as.numeric(as.character(probabilityMatrix[,"additive"]))))
  #This is our probability
  prob<-charactersFoundSum+charactersNotFoundSum 
  prob
}

#function to show which class is most probable on given text
predictResult=function(testMatrix,probabilityMatrixList){
  #input:
  #testMatrix (matrix)=doc term matrix made using function DocMatrixMake()
  #probabilityMatrixList (matrix)=probability matrix using function 
  #probabilityMatrix()
  #output:
  #name of a class which is most probable
  for(i in 1:nrow(testMatrix)) {
    #get words of test article
    testCharacters<-names(data.frame(
      as.matrix(testMatrix[i,as.matrix(testMatrix[i,])>0])))
    #calculate test set probabilities
    testProb=sapply(probabilityMatrixList, function(pMatrix,
                                                    testChars=testCharacters) {
      prob= getProbability(testChars,pMatrix)
      prob
    })
    #memorize article class which is most probable
    results[i]=names(testProb[testProb==max(testProb)])
  }
  results
}