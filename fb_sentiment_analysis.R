#merge the headlines an dthe direct comments on the headline id
temp<- merge(x=Headlines,y=directComments,x.by=Headlines$headline_id,y.by=directComments$headline_id)
#merge this temporary dataframe to the response comments
fb <- merge(x=temp,y=responseComment,by=c("headline_id","direct_comment_id"),all.x=TRUE)

#subset for Hong Kong Clashes
HKclash <- subset(fb, fb$news=='HK clash')

#subset for US 2020 elections
USelec <- subset(fb,fb$news=='US 2020')

#subset for ME crisis
MEcrisis <- subset(fb,fb$news=='ME Crisis')


#create term document matrices for all three issues

#HK clash
HK_headlines<-Headlines$headline[Headlines$news=="HK clash"]
HK_headlines<-removePunctuation(HK_headlines)
HK_headlines_source<-VectorSource(HK_headlines)
HK_headlines_corpus<-VCorpus(HK_headlines_source)
HK_headlines_corpus<-tm_map(HK_headlines_corpus,removeWords,stopwords("en"))
HK_headlines_corpus<-tm_map(HK_headlines_corpus,stripWhitespace)
HK_headlines_corpus<-tm_map(HK_headlines_corpus,removePunctuation)
HK_headlines_dtm<-DocumentTermMatrix(HK_headlines_corpus)
HK_headlines_tdm<-TermDocumentMatrix(HK_headlines_corpus)
HK_headlines_m<-as.matrix(HK_headlines_tdm)