#link
#https://www.r-bloggers.com/getting-started-with-twitter-analysis-in-r/
#https://github.com/timjurka/sentiment/blob/master/sentiment/R/classify_polarity.R
#https://ifordata.wordpress.com/2015/04/11/twitter-sentiment-analysis-in-r-two-ways-sentiment-package-and-reading-a-sentiment-score-file/

#nastavenie domovskeho priecinku
  setwd("~/TUKE_5.rocnik_ZS/Semanticky_a_socialny_web/Analysign_Autorship")

#nastavenia twittru
install.packages(c("devtools", "rjson", "bit64", "httr","httpuv"))
#kniznice
  library(devtools)
  library(twitteR)
  library(stringr)
  library(ggplot2)
  library(NLP)
  library(tm)
  library(RSentiment)
  library(RColorBrewer)
  library(wordcloud)


#apikey
  api_key <- "Z5PIfvkQExaVshBj0D58IKW1e"
#apisecret
  api_secret <- "Y53GB2Q0lNpXYW2qDETSSPHpSYBMpVFnGKjKL7BgSwyQyhmnG6"
#accesstoken
  access_token <- "938133282813829126-tavd9hUqPgR98IuLQ4w3NyM6iBweUMD"
#accesstokensecret
  access_token_secret <- " SAPDkfD5bOLd59H7mn24ndeXbXHdjyvaY6BHfCV78fOAO"
#nadviazanie spojenia 
  setup_twitter_oauth(api_key,api_secret)


#autor prispevku
  #pocet tweetov
  n=200 
  tweet='#xiaomi'

#zadavame tweet
  rdmTweets <- searchTwitter(tweet, n=n)
#vytvorenie datoveho ramca 
  datfram <- do.call("rbind", lapply(rdmTweets, as.data.frame))
#vsetky stlpce
  names(datfram)
#prve tri riadky
  head(datfram,3)
#obmedzenie pre zobrazenie viac ako jedneho vyskytu
  counts=table(datfram$screenName)
  cc=subset(counts,counts>1)
  barplot(cc,las=2,cex.names =0.6)
#odstranene pomocou znakovej sady 
  datfram$text=sapply(datfram$text,function(row) iconv(row,to='UTF-8'))
#pomocna funkcia na odstranenie @ pri mene...
  trim <- function (x) sub('@','',x)
##funkcie na pridanie novych stlpcov
  #vyberam kto ma spravu 
    datfram$to=sapply(datfram$text,function(tweet) str_extract(tweet,"^(@[[:alnum:]_]*)"))
    datfram$to=sapply(datfram$to,function(name) trim(name))
#sposob urcenia RT
  datfram$rt=sapply(datfram$text,function(tweet) trim(str_match(tweet,"^RT (@[[:alnum:]_]*)")[2]))
#vykreslenie poctu mien v grafe 
  ggplot()+geom_bar(aes(x=na.omit(datfram$rt)))+xlab(NULL)+theme(axis.text.x = element_text(angle=-45, hjust=0, vjust=1)) 
#koniec autora 


##analyza prispievatela 
  #vyberiem si meno prispievatela
    name="aadilv56"


#vyber textu twetu 
  data = as.data.frame.matrix(datfram[1], header=TRUE,quote=NULL) 
#vyberame tweet podla mena 
  data.rand <- data[grep(name, datfram$screenName),]
#funkcia na vypocet poctu slov
  countSpaces <- function(s) { sapply(gregexpr(" ", s), function(p) { sum(p>=0) } ) }
  foo <- transform(data.rand, baz = countSpaces(data.rand))
#odstranenie nevhodnych slov 
  data.rand=gsub("[^[:graph:]]"," ",data.rand)
  data.rand=gsub("https://t.co/","",data.rand)
  corpus <- Corpus(VectorSource(data.rand))
    corpus <- tm_map(corpus, removeNumbers)#cisla
    corpus <- tm_map(corpus, removePunctuation)#znaky
    corpus <- tm_map(corpus, stripWhitespace)#medzeri
    corpus <- tm_map(corpus, removeWords, stopwords('english'))#slova bez vyznamu
  matica <- DocumentTermMatrix(corpus)
#dlzka prispevku
  pris=foo[1,2]-matica$ncol
  word=foo[1,2]
#pocet komentov 
  pockom=sum(datfram$retweetCount[grep(name, datfram$screenName)])
#oblubenost
  oblub=sum(datfram$favoriteCount[grep(name, datfram$screenName)])
#cas prispevku
  caspri=datfram$created[grep(name, datfram$screenName)]
#krajina prispievatela 
  krajpris=datfram$language[grep(name, datfram$screenName)]
#priatel 
  pria=datfram$rt[grep(name, datfram$screenName)]
#pozitivne a negativny text
  dataframe <- data.frame(text=sapply(corpus, identity), stringsAsFactors=F)
  sentiment=calculate_sentiment(dataframe)
#workcloud
  wordcloud(corpus,scale=c(3,0.2),max.words = n/2, min.freq = 0.01,colors=brewer.pal(8, "Dark2"))
#koniec analyzy prispievatela


##vyhodnotenie autora
#nacitanie tabulky
  tabulka=read.table("Analyza.csv", header = FALSE, sep = ";",stringsAsFactors=FALSE)
#meno autora
  tabulka[1,2]=name
#twet
  tabulka[2,2]=tweet
#Ma komentare
  #malo #stredne #vela
  if(pockom<5){
    tabulka[3,2]="malo komentarov"
  }else if(pockom<15){
    tabulka[3,2]="stredne vela komentarov"
  }else{
    tabulka[3,2]="vela komentarov"
  }
#Pouziva slova
  #malo #stredne #vela
  if(word<10){
    tabulka[4,2]="malo slov"
  }else if(word<20){
    tabulka[4,2]="stredne vela slov"
  }else{
    tabulka[4,2]="vela slov"
  }
#Je oblubeny
  if(oblub<5){
    tabulka[5,2]="malo oblubeny"
  }else if(oblub<15){
    tabulka[5,2]="stredne oblubeny"
  }else{
    tabulka[5,2]="vela oblubeny"
  }
#Pridava prispevky
  #morning #day #night
  caspri=gsub(Sys.Date(), "" , caspri, perl=TRUE)
  caspri=gsub(" ", "" , caspri, perl=TRUE)
  caspri=gsub("[:*]", "" , caspri, perl=TRUE)
    if(as.integer(caspri)<90000){
      tabulka[6,2]="rano"
    }else if(as.integer(caspri)<190000){
      tabulka[6,2]="cez den"
    }else{
      tabulka[6,2]="vecer"
    }
#Je z krajiny
  if(krajpris=="en"){
    tabulka[7,2]="U.S.A"
  }else if(krajpris=="fr"){
    tabulka[7,2]="France"
  }else{
    tabulka[7,2]="other"
  }
#Ma priatela
  tabulka[8,2]=pria
#pozitivne a negativny text
  tabulka[9,2]=attributes(sentiment$sentiment)$levels
#koniec vyhodnotenia 

