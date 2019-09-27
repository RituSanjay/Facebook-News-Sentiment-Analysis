Headlines <- read_excel("C:/Users/ritu.susan.sanjay/Downloads/headlines.xlsx", 
                        sheet = "headlines")
directComments <- read_excel("C:/Users/ritu.susan.sanjay/Downloads/headlines.xlsx", 
                             sheet = "direct comments", col_types = c("text", 
                                                                      "text", "text"))
responseComment <- read_excel("C:/Users/ritu.susan.sanjay/Downloads/headlines.xlsx", 
                                  sheet = "response_comment", col_types = c("text", 
                                                                      "text", "text", "text"))

#merge the headlines an dthe direct comments on the headline id
temp<- merge(x=Headlines,y=directComments,x.by=Headlines$headline_id,y.by=directComments$headline_id)
#merge this temporary dataframe to the response comments
fb <- merge(x=temp,y=responseComment,by=c("headline_id","direct_comment_id"),all.x=TRUE)

stop_words<-c(stopwords("en"),"china","China","hong","Hong","kong","Kong",
              "will","the","can","police","people","america","americans",
              "american")

#subset for Hong Kong Clashes
HKclash <- subset(fb, fb$news=='HK clash')

#subset for US 2020 elections
USelec <- subset(fb,fb$news=='US 2020')

#subset for ME crisis
MEcrisis <- subset(fb,fb$news=='ME Crisis')


#create term document matrices for all three issues - no point in looking at
#a wordcloud for the headlines

#HK clash
HK_headlines<-Headlines$headline[Headlines$news=="HK clash"]
#HK_headlines<-removePunctuation(HK_headlines)
HK_headlines_source<-VectorSource(HK_headlines)
HK_headlines_corpus<-VCorpus(HK_headlines_source)
HK_headlines_corpus<-tm_map(HK_headlines_corpus,removeWords,stopwords("en"))
HK_headlines_corpus<-tm_map(HK_headlines_corpus,stripWhitespace)
HK_headlines_corpus<-tm_map(HK_headlines_corpus,removePunctuation)
#HK_headlines_dtm<-DocumentTermMatrix(HK_headlines_corpus)
HK_headlines_tdm<-TermDocumentMatrix(HK_headlines_corpus)
HK_headlines_m<-as.matrix(HK_headlines_tdm)

#lets look at the comments now:
#this includes both the direct and response comments
#to get unique direct comments use temp, filter by news=HK clash
#to get unique response comments use fb, filter by news=HK clash
direct<-temp$direct_comment[temp$news=="HK clash"]
response<-fb$response_comment[fb$news=="HK clash"]
response<-na.omit(response)
HK_comments<-c(direct,response)
#HK_comments<-na.omit(HK_comments)
HK_comments_source<-VectorSource(HK_comments)
HK_comments_corpus<-VCorpus(HK_comments_source)
HK_comments_corpus<-tm_map(HK_comments_corpus,stripWhitespace)
HK_comments_corpus<-tm_map(HK_comments_corpus,removePunctuation)
HK_comments_corpus<-tm_map(HK_comments_corpus,removeWords,stop_words)
HK_comments_tdm<-TermDocumentMatrix(HK_comments_corpus)
HK_comments_m<-as.matrix(HK_comments_tdm)
term_freq<-rowSums(HK_comments_m)
term_freq<-sort(term_freq,decreasing=TRUE)
terms_vec<-names(term_freq)
#this will help plot on a larger screen
dev.new(width = 1000, height = 1500, unit = "px")
wordcloud(terms_vec,term_freq,max.words=150,scale=c(4,.5),
          colors = c("red","grey80","mistyrose2","midnightblue"))
                           
#US Elections 2020
direct<-temp$direct_comment[temp$news=="US 2020"]
response<-fb$response_comment[fb$news=="US 2020"]
response<-na.omit(response)
US_comments<-c(direct,response)
US_comments_source<-VectorSource(US_comments)
US_comments_corpus<-VCorpus(US_comments_source)
US_comments_corpus<-tm_map(US_comments_corpus,stripWhitespace)
US_comments_corpus<-tm_map(US_comments_corpus,removePunctuation)
US_comments_corpus<-tm_map(US_comments_corpus,removeWords,stop_words)
US_comments_tdm<-TermDocumentMatrix(US_comments_corpus)
US_comments_m<-as.matrix(US_comments_tdm)
term_freq<-rowSums(US_comments_m)
term_freq<-sort(term_freq,decreasing=TRUE)
terms_vec<-names(term_freq)
dev.new(width = 1000, height = 1500, unit = "px")
wordcloud(terms_vec,term_freq,max.words=150,scale=c(4,.5),
          colors = c("red","grey80","mistyrose2","midnightblue"))

#ME crisis
direct<-temp$direct_comment[temp$news=="ME Crisis"]
response<-fb$response_comment[fb$news=="ME Crisis"]
response<-na.omit(response)
ME_comments<-c(direct,response)
ME_comments_source<-VectorSource(ME_comments)
ME_comments_corpus<-VCorpus(ME_comments_source)
ME_comments_corpus<-tm_map(ME_comments_corpus,stripWhitespace)
ME_comments_corpus<-tm_map(ME_comments_corpus,removePunctuation)
ME_comments_corpus<-tm_map(ME_comments_corpus,removeWords,stop_words)
ME_comments_tdm<-TermDocumentMatrix(ME_comments_corpus)
ME_comments_m<-as.matrix(ME_comments_tdm)
term_freq<-rowSums(ME_comments_m)
term_freq<-sort(term_freq,decreasing=TRUE)
terms_vec<-names(term_freq)
dev.new(width = 1000, height = 1500, unit = "px")
wordcloud(terms_vec,term_freq,max.words=150,scale=c(4,.5),
          colors = c("red","grey80","mistyrose2","midnightblue"))



#now that we've got the whole wordcloud part out of the way
#lets get down to the sentiment analysis part of it

#first we'll analyze the headlines, then check the sentiments of the comments
#and finally the response comments

