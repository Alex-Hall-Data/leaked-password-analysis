library("plyr", lib.loc="~/R/win-library/3.2")
library("rjson", lib.loc="~/R/win-library/3.2")

raw<-read.csv("C:/Users/Alex/Documents/R/password analysis/raw.csv")

#get location data (originally from the function below and 'last_ip' from the raw data)
location_data<-read.csv("C:/Users/Alex/Documents/R/password analysis/locations.csv")

dictionary1 <- read.csv("C:/Users/Alex/Documents/R/password analysis/words.csv")

#convert dictionary and relevant columns to vectors
passwords<-as.vector(raw$plain_password)
username<-as.vector(raw$username)
email<-as.vector(raw$email)
firstname<-as.vector(raw$firstname)
lastname<-as.vector(raw$lastname)
last_ip<-as.vector(raw$last_ip)
dictionary1<-as.vector(dictionary1[,1])

#process ip data:
#replace non-valid ip addresses with blank entries
re <- regexpr("(?(?=.*?(\\d+\\.\\d+\\.\\d+\\.\\d+).*?)(\\1|))", last_ip, perl = TRUE)
last_ip<-regmatches(last_ip, re)

#function to convert ip address to lat long (can use if want to re-process ip addresses)
freegeoip <- function(ip, format = ifelse(length(ip)==1,'list','dataframe'))
{
  if (1 == length(ip))
  {
    # a single IP address
    require(rjson)
    url <- paste(c("http://freegeoip.net/json/", ip), collapse='')
    ret <- fromJSON(readLines(url, warn=FALSE))
    if (format == 'dataframe')
      ret <- data.frame(t(unlist(ret)))
    return(ret)
  } else {
    ret <- data.frame()
    for (i in 1:length(ip))
    {
      r <- freegeoip(ip[i], format="dataframe")
      ret <- rbind(ret, r)
    }
    return(ret)
  }
}  

#dictionary search on all paswords
single_word_passwords<-match(passwords,dictionary1)
c<-sum(is.na(single_word_passwords))
#boolean list for single word passwords
single_word_password_logical<-passwords %in% dictionary1

#total number of passwords found by dictionary search
single_word_count<-length(single_word_passwords)-c


#find only lowercase letter passwords - gives vector of locations of all lowercase passwords:
lowercase_passwords<-grep("^[a-z]+$",passwords)
lowercase_password_logical<-grepl("^[a-z]+$",passwords)
lowercase_count<-length(lowercase_passwords)

#find only lowercase letter passwords - gives vector of locations of all text only passwords:
text_passwords<-grep("^[a-zA-Z]+$",passwords)
text_password_logical<-grepl("^[a-zA-Z]+$",passwords)
text_count<-length(text_passwords)


#find all passwords that CONTAIN a dictionary word:
#for each word in dictionary - return password that contain it:
#dictionary_in_passwords<- lapply(dictionary1, function(x) which(grepl(x, passwords, fixed=TRUE))) #TAKES AGES TO RUN -COMMENT OUT IF REQUIRED
#dictionary_in_count<-length(dictionary_in_passwords)

#this gives number of times each word in the dictionary appears in the password list:
num<-lapply(dictionary_in_passwords, function(x) length(gregexpr(" ",x)))
#make a column of indices for the above
indices<-c(1:length(dictionary_in_passwords))
#make into dataframe
counts<-cbind(indices,num)
#sort by frequency of word appearance:
counts<-counts[order(-as.numeric(num)),]
#get words from dictionary with the above indices (ie top 1000 used words)
common_dict_words<-dictionary1[as.numeric(counts[,1][1:1000])]


#find most common passwords
common_passwords<-sort(table(passwords),decreasing=TRUE)[1:50]

#process the location data
#this gets rid of my ip (was subbed in for invalid ip addresses) and leaves only US IP addresses
US_locations<-location_data[which(location_data$country_code=="US"),]
#plot locations
plot(US_locations$longitude,US_locations$latitude,xlim=c(-170,-60),ylim=c(15,65),pch=20,col="red")


#make a list of logicals where 

#name everything xxxx_logical to put into a dataframe for apriori
#have, single_word_password_logical, lowercase_password_logical, text_password_logical, 
#use apriori on results of regex matches, metro code (from location_data), username, email
