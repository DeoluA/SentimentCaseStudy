##  Sentiment Analysis - April 11 2015 Nigerian Presidential Elections ##
#  By Deolu Adeleye (deoluadeleye@gmail.com)

#This script was used to produce the report, and is completely reproducible by anyone.

# Disclosure is same as in other file

#### CODE BEGINS ####
#check if necessary packages are installed. If they are, load them; else, install and load them.
reqd_pkgs <- c("dplyr", "plyr", "ggplot2", "devtools", "NLP", "tm", "SnowballC", "RWeka", "Rstem","RColorBrewer", "stringr", "lubridate")
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


####download tweets and lexicons dataset####
download.file(url="https://www.dropbox.com/s/z7gqrj8hgfehu6g/gov-sentiment-analysis.zip?dl=1",
              destfile="gov-sentiment-analysis.zip")
##unzip it
unzip(zipfile="gov-sentiment-analysis.zip",
      exdir = "gov-sentiment-analysis")

#set working directory to newly downloaded folder
setwd("gov-sentiment-analysis")

subjectivity <- read.csv(system.file("data/subjectivity.csv.gz",package="sentiment"),header=F,stringsAsFactors=F)

names(subjectivity)<-c("word","strength","polarity")

pos.words <- read.table("gov-sentiment-analysis/positive-words.txt", stringsAsFactors=F, skip=35)
pos.words[,2] <- rep("subj", nrow(pos.words))
pos.words[,3] <- rep("positive", nrow(pos.words))
names(pos.words)<-names(subjectivity)
neg.words <- read.table("gov-sentiment-analysis/negative-words.txt", stringsAsFactors=F, skip=35)
neg.words[,2] <- rep("subj", nrow(neg.words))
neg.words[,3] <- rep("negative", nrow(neg.words))
names(neg.words)<-names(subjectivity)
#merge
subjectivity <- rbind(subjectivity,pos.words,neg.words)

#load functions
source("gov-sentiment-analysis/new_classify_polarity.R")
source("gov-sentiment-analysis/score.sentiment.R")

####load tweets####
all_ambode_tweets<-read.csv("gov-sentiment-analysis/ambode_tweets_09.csv",stringsAsFactors=F, colClasses=c("character","POSIXct"))
all_agbaje_tweets<-read.csv("gov-sentiment-analysis/agbaje_tweets_09.csv",stringsAsFactors=F, colClasses=c("character","POSIXct"))

####SENTIMENTS####
#all_ambode case
all_ambode_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", all_ambode_tweets[,1])
# remove at people
all_ambode_txt = gsub("@\\w+", "", all_ambode_txt)
# remove punctuation
all_ambode_txt = gsub("[[:punct:]]", "", all_ambode_txt)
# remove numbers
all_ambode_txt = gsub("[[:digit:]]", "", all_ambode_txt)
# remove html links
all_ambode_txt = gsub("http\\w+", "", all_ambode_txt)
# remove unnecessary spaces
all_ambode_txt = gsub("[ \t]{2,}", "", all_ambode_txt)
all_ambode_txt = gsub("^\\s+|\\s+$", "", all_ambode_txt)

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
all_ambode_txt = sapply(all_ambode_txt, try.error)

#remove stopwords
all_ambode_created = all_ambode_tweets[,2]
all_ambode_created = all_ambode_created[!all_ambode_txt %in% stopwords("SMART")]
all_ambode_txt <- all_ambode_txt[!all_ambode_txt %in% stopwords("SMART")]

# remove NAs in all_ambode_txt
all_ambode_txt = all_ambode_txt[!is.na(all_ambode_txt)]
names(all_ambode_txt) = NULL

# classify emotion with bayes
class_emo_bayes = classify_emotion(all_ambode_txt, algorithm="bayes", prior=1.0)
# classify emotion with voter
class_emo_voter = classify_emotion(all_ambode_txt, algorithm="voter", prior=1.0)
# get bayes_emotion best fit
bayes_emotion = class_emo_bayes[,7]
# get voter_emotion best fit
voter_emotion = class_emo_voter[,7]
# substitute NA's by "unknown"
bayes_emotion[is.na(bayes_emotion)] = "unknown"
# substitute NA's by "unknown"
voter_emotion[is.na(voter_emotion)] = "unknown"

# classify polarity with bayes
class_pol_bayes = new_classify_polarity(all_ambode_txt, algorithm="bayes")
# classify polarity with voter
class_pol_voter = new_classify_polarity(all_ambode_txt, algorithm="voter")
# get bayes polarity best fit
bayes_polarity = class_pol_bayes[,4]
# get voter polarity best fit
voter_polarity = class_pol_voter[,4]

#bayes data frame
all_ambode_df_bayes = data.frame(text=all_ambode_txt, bayes_emotion=bayes_emotion, bayes_polarity=bayes_polarity, Candidate=rep("Ambode",length(all_ambode_txt)), stringsAsFactors=FALSE)
# sort data frame
all_ambode_df_bayes = within(all_ambode_df_bayes, bayes_emotion <- factor(bayes_emotion, levels=names(sort(table(bayes_emotion),decreasing=TRUE))))
#voter data frame
all_ambode_df_voter = data.frame(text=all_ambode_txt, voter_emotion=voter_emotion, voter_polarity=voter_polarity, Candidate=rep("Ambode",length(all_ambode_txt)), stringsAsFactors=FALSE)
# sort data frame
all_ambode_df_voter = within(all_ambode_df_voter, voter_emotion <- factor(voter_emotion, levels=names(sort(table(voter_emotion),decreasing=TRUE))))


# sentiment score
all_ambode = score.sentiment(all_ambode_txt, subjectivity$word[subjectivity$polarity=="positive"], subjectivity$word[subjectivity$polarity=="negative"])
all_ambode$Candidate = rep("Ambode",nrow(all_ambode))


#all_agbaje case
all_agbaje_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", all_agbaje_tweets[,1])
# remove at people
all_agbaje_txt = gsub("@\\w+", "", all_agbaje_txt)
# remove punctuation
all_agbaje_txt = gsub("[[:punct:]]", "", all_agbaje_txt)
# remove numbers
all_agbaje_txt = gsub("[[:digit:]]", "", all_agbaje_txt)
# remove html links
all_agbaje_txt = gsub("http\\w+", "", all_agbaje_txt)
# remove unnecessary spaces
all_agbaje_txt = gsub("[ \t]{2,}", "", all_agbaje_txt)
all_agbaje_txt = gsub("^\\s+|\\s+$", "", all_agbaje_txt)

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
all_agbaje_txt = sapply(all_agbaje_txt, try.error)

#remove stopwords
all_agbaje_created = all_agbaje_tweets[,2]
all_agbaje_created = all_agbaje_created[!all_agbaje_txt %in% stopwords("SMART")]
all_agbaje_txt <- all_agbaje_txt[!all_agbaje_txt %in% stopwords("SMART")]

# remove NAs in all_agbaje_txt
all_agbaje_created = all_agbaje_created[!is.na(all_agbaje_txt)]
all_agbaje_txt = all_agbaje_txt[!is.na(all_agbaje_txt)]
names(all_agbaje_txt) = NULL

# classify emotion with bayes
class_emo_bayes = classify_emotion(all_agbaje_txt, algorithm="bayes", prior=1.0)
# classify emotion with voter
class_emo_voter = classify_emotion(all_agbaje_txt, algorithm="voter", prior=1.0)
# get bayes_emotion best fit
bayes_emotion = class_emo_bayes[,7]
# get voter_emotion best fit
voter_emotion = class_emo_voter[,7]
# substitute NA's by "unknown"
bayes_emotion[is.na(bayes_emotion)] = "unknown"
# substitute NA's by "unknown"
voter_emotion[is.na(voter_emotion)] = "unknown"

# classify polarity with bayes
class_pol_bayes = new_classify_polarity(all_agbaje_txt, algorithm="bayes")
# classify polarity with voter
class_pol_voter = new_classify_polarity(all_agbaje_txt, algorithm="voter")
# get bayes polarity best fit
bayes_polarity = class_pol_bayes[,4]
# get voter polarity best fit
voter_polarity = class_pol_voter[,4]

#bayes data frame
all_agbaje_df_bayes = data.frame(text=all_agbaje_txt, bayes_emotion=bayes_emotion, bayes_polarity=bayes_polarity, Candidate=rep("Agbaje",length(all_agbaje_txt)), stringsAsFactors=FALSE)
# sort data frame
all_agbaje_df_bayes = within(all_agbaje_df_bayes, bayes_emotion <- factor(bayes_emotion, levels=names(sort(table(bayes_emotion),decreasing=TRUE))))
#voter data frame
all_agbaje_df_voter = data.frame(text=all_agbaje_txt, voter_emotion=voter_emotion, voter_polarity=voter_polarity, Candidate=rep("Agbaje",length(all_agbaje_txt)), stringsAsFactors=FALSE)
# sort data frame
all_agbaje_df_voter = within(all_agbaje_df_voter, voter_emotion <- factor(voter_emotion, levels=names(sort(table(voter_emotion),decreasing=TRUE))))

# sentiment score
all_agbaje = score.sentiment(all_agbaje_txt, subjectivity$word[subjectivity$polarity=="positive"], subjectivity$word[subjectivity$polarity=="negative"])
all_agbaje$Candidate = rep("Agbaje",nrow(all_agbaje))



#merge all the results
sentiments_bayes <- rbind(all_ambode_df_bayes,all_agbaje_df_bayes)
sentiments_voter <- rbind(all_ambode_df_voter,all_agbaje_df_voter)
results <- rbind(all_ambode,all_agbaje)

#### Plots ####
#view the plots
ggplot(sentiments_bayes, aes(x=bayes_polarity)) + geom_bar(aes(y=..count.., fill=bayes_polarity), position="dodge") + scale_fill_brewer(palette="Dark2") + labs(x="Polarity categories", y="Number of Tweets",title="Using Naive Bayes Algorithm") + facet_grid(.~Candidate)

ggplot(sentiments_bayes, aes(x=bayes_emotion)) + geom_bar(aes(y=..count.., fill=bayes_emotion), position="dodge") + scale_fill_brewer(palette="Dark2") + labs(x="Emotion categories", y="Number of Tweets", title="Emotions Evoked (Using Naive Bayes Algorithm)") + facet_grid(.~Candidate)

ggplot(sentiments_voter, aes(x=voter_polarity)) + geom_bar(aes(y=..count.., fill=voter_polarity), position="dodge") + scale_fill_brewer(palette="Dark2") + labs(x="Polarity categories", y="Number of Tweets",title="Using Simple Voter Algorithm") + facet_grid(.~Candidate)

ggplot(sentiments_voter, aes(x=voter_emotion)) + geom_bar(aes(y=..count.., fill=voter_emotion), position="dodge") + scale_fill_brewer(palette="Dark2") + labs(x="Emotion categories", y="Number of Tweets", title="Emotions Evoked (Using Simple Voter Algorithm)") + facet_grid(.~Candidate)

#Sentiment Scores
ggplot(data=results, mapping=aes(x=score, fill=Candidate)) + geom_bar(binwidth=1) + labs(x="Scores", y="Number of Tweets", title="Sentiment Scores Per Candidate") + facet_grid(Candidate~.)

#Cumulative scores
total_all_ambode <- data.frame(score=sum(all_ambode$score), Candidate=rep("Ambode",length(sum(all_ambode$score))))
total_all_agbaje <- data.frame(score=sum(all_agbaje$score), Candidate=rep("Agbaje",length(sum(all_agbaje$score))))


ggplot() +
  geom_bar(data=total_all_ambode,
           mapping=aes(x=Candidate, y=score),
           binwidth=10, position="dodge",stat="identity", fill="red") +
  geom_bar(data=total_all_agbaje, mapping=aes(x=Candidate, y=score),
           binwidth=10, position="dodge",stat="identity", fill="green") +
  labs(x="Candidate", y="Score", title="Total Sentiment Scores Tallied")


#### Plot of per day ####

ambode_df <- cbind(all_ambode,all_ambode_created)
names(ambode_df)[4] <- c("period")
ambode_df$period <- paste(month(ambode_df$period),"-",day(ambode_df$period),"-",year(ambode_df$period),sep="")
ambode_df <- tbl_df(ambode_df)

d_ambode_df <- group_by(ambode_df, period)
s_ambode_df <- dplyr::summarize(d_ambode_df, daily=sum(score))


agbaje_df <- cbind(all_agbaje,all_agbaje_created)
names(agbaje_df)[4] <- c("period")
agbaje_df$period <- paste(month(agbaje_df$period),"-",day(agbaje_df$period),"-",year(agbaje_df$period),sep="")
agbaje_df <- tbl_df(agbaje_df)

d_agbaje_df <- group_by(agbaje_df, period)
s_agbaje_df <- dplyr::summarize(d_agbaje_df, daily=sum(score))

all_df <- rbind(s_ambode_df,s_agbaje_df)

all_df$Candidate <- c(rep("Ambode",8),rep("Agbaje",8))

qplot(data=all_df, x=period, y=daily, fill=Candidate, geom="bar", stat="identity", position="dodge") +
  labs(x="Days", y="Score", title="Score Over Days\n(Today shown first)")
