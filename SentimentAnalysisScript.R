##  Sentiment Analysis - March 28 2015 Nigerian Presidential Elections ##
#  By Deolu Adeleye (deoluadeleye@gmail.com)

#This script was used to produce the report ..., and is completely by anyone reproducible.

#For full disclosure, the original report was generated on the following system with the following specifications:

# System:
# - 'HP Compaq Presario CQ61 Notebook PC'
# - Pentium(R) Dual-Core CPU T4300 @ 2.10GHz (2 CPUs), ~2.1GHz
# - 2048MB RAM
# - OS: 'Windows 7 Ultimate 32-bit (6.1, Build 7600) (7600.win7_rtm.090713-1255)'
# (if you're using Windows, you can generate your own specifications by typing 'dxdiag' in your Windows Start-up menu)
# R:
# - R version: '3.1.1 (2014-07-10) -- "Sock it to Me", on platform 'i386-w64-mingw32/i386 (32-bit)'
# - RStudio version: '0.98.1074', mode 'Desktop'

# (you can generate your R version info by typing any of the following:
# - 'Sys.getenv()'
# - 'R.Version()'
# - 'rstudio::versionInfo()'
# - 'sessionInfo()'
# in your console)


#CODE BEGINS#
#check if necessary packages are installed. If they are, load them; else, install and load them.
reqd_pkgs <- c("dplyr", "plyr", "ggplot2", "devtools", "NLP", "tm", "SnowballC", "RWeka", "Rstem","RColorBrewer", "stringr")
for (i in 1:length(reqd_pkgs))
{
  if (! (reqd_pkgs[i] %in% rownames(installed.packages())))
  {
    paste("Package '",reqd_pkgs[i],"' not installed. Installing...", sep="")
    install.packages(reqd_pkgs[i])
    library(reqd_pkgs[i], character.only=T)
  }
  else library(reqd_pkgs[i], character.only=T)
}

#download 'twitteR' from Jeff Gentry's GitHub repo, instead of CRAN (author's repos are usually more up to date)
devtools::install_github("geoffjentry/twitteR")

library(twitteR)

#download 'Sentiment' package from CRAN archive
download.file(url="http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz",
              destfile="sentiment_0.2.tar.gz")
install.packages("sentiment_0.2.tar.gz", repos = NULL, type = "source")

library(sentiment)


#download tweets and lexicons dataset
download.file(url="https://www.dropbox.com/s/mqdpdkz7cywklby/sentiment-analysis.zip?dl=1",
              destfile="sentiment-analysis.zip")
##unzip it
unzip(zipfile="sentiment-analysis.zip",
      exdir = "sentiment-analysis")

#load tweets
buhari_tweets<-read.csv("sentiment-analysis/buhari_tweets.csv",stringsAsFactors=F)
jonathan_tweets<-read.csv("sentiment-analysis/jonathan_tweets.csv",stringsAsFactors=F)
sonaiya_tweets<-read.csv("sentiment-analysis/sonaiya_tweets.csv",stringsAsFactors=F)

#load lexicons
subjectivity <- read.csv(system.file("data/subjectivity.csv.gz",package="sentiment"),header=F,stringsAsFactors=F)

names(subjectivity)<-c("word","strength","polarity")

pos.words <- read.table("sentiment-analysis/positive-words.txt", stringsAsFactors=F, skip=35)
pos.words[,2] <- rep("subj", nrow(pos.words))
pos.words[,3] <- rep("positive", nrow(pos.words))
names(pos.words)<-names(subjectivity)
neg.words <- read.table("sentiment-analysis/negative-words.txt", stringsAsFactors=F, skip=35)
neg.words[,2] <- rep("subj", nrow(neg.words))
neg.words[,3] <- rep("negative", nrow(neg.words))
names(neg.words)<-names(subjectivity)
#merge
subjectivity <- rbind(subjectivity,pos.words,neg.words)

#load functions
source("sentiment-analysis/new_classify_polarity.R")
source("sentiment-analysis/score.sentiment.R")

#buhari case
buhari_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", buhari_tweets[,1])
# remove at people
buhari_txt = gsub("@\\w+", "", buhari_txt)
# remove punctuation
buhari_txt = gsub("[[:punct:]]", "", buhari_txt)
# remove numbers
buhari_txt = gsub("[[:digit:]]", "", buhari_txt)
# remove html links
buhari_txt = gsub("http\\w+", "", buhari_txt)
# remove unnecessary spaces
buhari_txt = gsub("[ \t]{2,}", "", buhari_txt)
buhari_txt = gsub("^\\s+|\\s+$", "", buhari_txt)

# define "tolower error handling" function
try.error = function(x)
{
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x),
                       error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}
#lower case using try.error with sapply
buhari_txt = sapply(buhari_txt, try.error)

#remove stopwords
buhari_txt <- buhari_txt[!buhari_txt %in% stopwords("SMART")]

# remove NAs in buhari_txt
buhari_txt = buhari_txt[!is.na(buhari_txt)]
names(buhari_txt) = NULL

# classify emotion with bayes
class_emo_bayes = classify_emotion(buhari_txt, algorithm="bayes", prior=1.0)
# classify emotion with voter
class_emo_voter = classify_emotion(buhari_txt, algorithm="voter", prior=1.0)
# get bayes_emotion best fit
bayes_emotion = class_emo_bayes[,7]
# get voter_emotion best fit
voter_emotion = class_emo_voter[,7]
# substitute NA's by "unknown"
bayes_emotion[is.na(bayes_emotion)] = "unknown"
# substitute NA's by "unknown"
voter_emotion[is.na(voter_emotion)] = "unknown"

# classify polarity with bayes
class_pol_bayes = new_classify_polarity(buhari_txt, algorithm="bayes")
# classify polarity with voter
class_pol_voter = new_classify_polarity(buhari_txt, algorithm="voter")
# get bayes polarity best fit
bayes_polarity = class_pol_bayes[,4]
# get voter polarity best fit
voter_polarity = class_pol_voter[,4]

#bayes data frame
buhari_df_bayes = data.frame(text=buhari_txt, bayes_emotion=bayes_emotion, bayes_polarity=bayes_polarity, Candidate=rep("Buhari",length(buhari_txt)), stringsAsFactors=FALSE)
# sort data frame
buhari_df_bayes = within(buhari_df_bayes, bayes_emotion <- factor(bayes_emotion, levels=names(sort(table(bayes_emotion),decreasing=TRUE))))
#voter data frame
buhari_df_voter = data.frame(text=buhari_txt, voter_emotion=voter_emotion, voter_polarity=voter_polarity, Candidate=rep("Buhari",length(buhari_txt)), stringsAsFactors=FALSE)
# sort data frame
buhari_df_voter = within(buhari_df_voter, voter_emotion <- factor(voter_emotion, levels=names(sort(table(voter_emotion),decreasing=TRUE))))


# sentiment score
buhari = score.sentiment(buhari_txt, subjectivity$word[subjectivity$polarity=="positive"], subjectivity$word[subjectivity$polarity=="negative"])
buhari$Candidate = rep("Buhari",nrow(buhari))


#jonathan case
jonathan_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", jonathan_tweets[,1])
# remove at people
jonathan_txt = gsub("@\\w+", "", jonathan_txt)
# remove punctuation
jonathan_txt = gsub("[[:punct:]]", "", jonathan_txt)
# remove numbers
jonathan_txt = gsub("[[:digit:]]", "", jonathan_txt)
# remove html links
jonathan_txt = gsub("http\\w+", "", jonathan_txt)
# remove unnecessary spaces
jonathan_txt = gsub("[ \t]{2,}", "", jonathan_txt)
jonathan_txt = gsub("^\\s+|\\s+$", "", jonathan_txt)

# define "tolower error handling" function
try.error = function(x)
{
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x),
                       error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}
#lower case using try.error with sapply
jonathan_txt = sapply(jonathan_txt, try.error)

#remove stopwords
jonathan_txt <- jonathan_txt[!jonathan_txt %in% stopwords("SMART")]

# remove NAs in jonathan_txt
jonathan_txt = jonathan_txt[!is.na(jonathan_txt)]
names(jonathan_txt) = NULL

# classify emotion with bayes
class_emo_bayes = classify_emotion(jonathan_txt, algorithm="bayes", prior=1.0)
# classify emotion with voter
class_emo_voter = classify_emotion(jonathan_txt, algorithm="voter", prior=1.0)
# get bayes_emotion best fit
bayes_emotion = class_emo_bayes[,7]
# get voter_emotion best fit
voter_emotion = class_emo_voter[,7]
# substitute NA's by "unknown"
bayes_emotion[is.na(bayes_emotion)] = "unknown"
# substitute NA's by "unknown"
voter_emotion[is.na(voter_emotion)] = "unknown"

# classify polarity with bayes
class_pol_bayes = new_classify_polarity(jonathan_txt, algorithm="bayes")
# classify polarity with voter
class_pol_voter = new_classify_polarity(jonathan_txt, algorithm="voter")
# get bayes polarity best fit
bayes_polarity = class_pol_bayes[,4]
# get voter polarity best fit
voter_polarity = class_pol_voter[,4]

#bayes data frame
jonathan_df_bayes = data.frame(text=jonathan_txt, bayes_emotion=bayes_emotion, bayes_polarity=bayes_polarity, Candidate=rep("Jonathan",length(jonathan_txt)), stringsAsFactors=FALSE)
# sort data frame
jonathan_df_bayes = within(jonathan_df_bayes, bayes_emotion <- factor(bayes_emotion, levels=names(sort(table(bayes_emotion),decreasing=TRUE))))
#voter data frame
jonathan_df_voter = data.frame(text=jonathan_txt, voter_emotion=voter_emotion, voter_polarity=voter_polarity, Candidate=rep("Jonathan",length(jonathan_txt)), stringsAsFactors=FALSE)
# sort data frame
jonathan_df_voter = within(jonathan_df_voter, voter_emotion <- factor(voter_emotion, levels=names(sort(table(voter_emotion),decreasing=TRUE))))

# sentiment score
jonathan = score.sentiment(jonathan_txt, subjectivity$word[subjectivity$polarity=="positive"], subjectivity$word[subjectivity$polarity=="negative"])
jonathan$Candidate = rep("Jonathan",nrow(jonathan))


#sonaiya case
sonaiya_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", sonaiya_tweets[,1])
# remove at people
sonaiya_txt = gsub("@\\w+", "", sonaiya_txt)
# remove punctuation
sonaiya_txt = gsub("[[:punct:]]", "", sonaiya_txt)
# remove numbers
sonaiya_txt = gsub("[[:digit:]]", "", sonaiya_txt)
# remove html links
sonaiya_txt = gsub("http\\w+", "", sonaiya_txt)
# remove unnecessary spaces
sonaiya_txt = gsub("[ \t]{2,}", "", sonaiya_txt)
sonaiya_txt = gsub("^\\s+|\\s+$", "", sonaiya_txt)

# define "tolower error handling" function
try.error = function(x)
{
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x),
                       error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}
#lower case using try.error with sapply
sonaiya_txt = sapply(sonaiya_txt, try.error)

#remove stopwords
sonaiya_txt <- sonaiya_txt[!sonaiya_txt %in% stopwords("SMART")]

# remove NAs in sonaiya_txt
sonaiya_txt = sonaiya_txt[!is.na(sonaiya_txt)]
names(sonaiya_txt) = NULL

# classify emotion with bayes
class_emo_bayes = classify_emotion(sonaiya_txt, algorithm="bayes", prior=1.0)
# classify emotion with voter
class_emo_voter = classify_emotion(sonaiya_txt, algorithm="voter", prior=1.0)
# get bayes_emotion best fit
bayes_emotion = class_emo_bayes[,7]
# get voter_emotion best fit
voter_emotion = class_emo_voter[,7]
# substitute NA's by "unknown"
bayes_emotion[is.na(bayes_emotion)] = "unknown"
# substitute NA's by "unknown"
voter_emotion[is.na(voter_emotion)] = "unknown"

# classify polarity with bayes
class_pol_bayes = new_classify_polarity(sonaiya_txt, algorithm="bayes")
# classify polarity with voter
class_pol_voter = new_classify_polarity(sonaiya_txt, algorithm="voter")
# get bayes polarity best fit
bayes_polarity = class_pol_bayes[,4]
# get voter polarity best fit
voter_polarity = class_pol_voter[,4]

#bayes data frame
sonaiya_df_bayes = data.frame(text=sonaiya_txt, bayes_emotion=bayes_emotion, bayes_polarity=bayes_polarity, Candidate=rep("Sonaiya",length(sonaiya_txt)), stringsAsFactors=FALSE)
# sort data frame
sonaiya_df_bayes = within(sonaiya_df_bayes, bayes_emotion <- factor(bayes_emotion, levels=names(sort(table(bayes_emotion),decreasing=TRUE))))
#voter data frame
sonaiya_df_voter = data.frame(text=sonaiya_txt, voter_emotion=voter_emotion, voter_polarity=voter_polarity, Candidate=rep("Sonaiya",length(sonaiya_txt)), stringsAsFactors=FALSE)
# sort data frame
sonaiya_df_voter = within(sonaiya_df_voter, voter_emotion <- factor(voter_emotion, levels=names(sort(table(voter_emotion),decreasing=TRUE))))

# sentiment score
sonaiya = score.sentiment(sonaiya_txt, subjectivity$word[subjectivity$polarity=="positive"], subjectivity$word[subjectivity$polarity=="negative"])
sonaiya$Candidate = rep("Sonaiya",nrow(sonaiya))


#merge all the results
sentiments_bayes <- rbind(buhari_df_bayes,jonathan_df_bayes,sonaiya_df_bayes)
sentiments_voter <- rbind(buhari_df_voter,jonathan_df_voter,sonaiya_df_voter)
results <- rbind(buhari,jonathan,sonaiya)

#view the plots
ggplot(sentiments_bayes, aes(x=bayes_polarity)) + geom_bar(aes(y=..count.., fill=bayes_polarity), position="dodge") + scale_fill_brewer(palette="Dark2") + labs(x="Polarity categories", y="Number of Tweets",title="Using Naive Bayes Algorithm") + facet_grid(.~Candidate)

ggplot(sentiments_bayes, aes(x=bayes_emotion)) + geom_bar(aes(y=..count.., fill=bayes_emotion), position="dodge") + scale_fill_brewer(palette="Dark2") + labs(x="Emotion categories", y="Number of Tweets", title="Emotions Evoked (Using Naive Bayes Algorithm)") + facet_grid(.~Candidate)

ggplot(sentiments_voter, aes(x=voter_polarity)) + geom_bar(aes(y=..count.., fill=voter_polarity), position="dodge") + scale_fill_brewer(palette="Dark2") + labs(x="Polarity categories", y="Number of Tweets",title="Using Simple Voter Algorithm") + facet_grid(.~Candidate)

ggplot(sentiments_voter, aes(x=voter_emotion)) + geom_bar(aes(y=..count.., fill=voter_emotion), position="dodge") + scale_fill_brewer(palette="Dark2") + labs(x="Emotion categories", y="Number of Tweets", title="Emotions Evoked (Using Simple Voter Algorithm)") + facet_grid(.~Candidate)

#Sentiment Scores
ggplot(data=results, mapping=aes(x=score, fill=Candidate)) + geom_bar(binwidth=1) + labs(x="Scores", y="Number of Tweets", title="Sentiment Scores Per Candidate") + facet_grid(Candidate~.)

#Cumulative scores
total_buhari <- data.frame(score=sum(buhari$score), Candidate=rep("Buhari",length(sum(buhari$score))))
total_jonathan <- data.frame(score=sum(jonathan$score), Candidate=rep("Jonathan",length(sum(jonathan$score))))
total_sonaiya <- data.frame(score=sum(sonaiya$score), Candidate=rep("Sonaiya",length(sum(sonaiya$score))))


ggplot() +
  geom_bar(data=total_buhari,
           mapping=aes(x=Candidate, y=score),
           binwidth=10, position="dodge",stat="identity", fill="red") +
  geom_bar(data=total_jonathan, mapping=aes(x=Candidate, y=score),
           binwidth=10, position="dodge",stat="identity", fill="yellow") +
  geom_bar(data=total_sonaiya, mapping=aes(x=Candidate, y=score),
           binwidth=10, position="dodge",stat="identity", fill="green") +
  labs(x="Candidate", y="Score", title="Total Sentiment Scores Tallied")

