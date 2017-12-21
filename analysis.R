library("plyr", lib.loc="~/R/win-library/3.2")
library("rjson", lib.loc="~/R/win-library/3.2")
library("arules", lib.loc="~/R/win-library/3.2")
library("arulesViz", lib.loc="~/R/win-library/3.2")


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
signup_date<-as.vector(raw$signup_date)
last_signin<-as.vector(raw$last_access_time)
dictionary1<-as.vector(dictionary1[,1])

#only use dictionary words of length 5 or more
dictionary_nchar<-lapply(dictionary1,nchar)
dictionary_nchar<-as.numeric(dictionary_nchar)
dictionary_data<-cbind(dictionary1,dictionary_nchar)
dictionary_data<-dictionary_data[as.numeric(dictionary_data[,2])>4,]
dictionary1<-as.vector(dictionary_data[,1])

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

#find only text letter passwords - gives vector of locations of all text only passwords:
text_passwords<-grep("^[a-zA-Z]+$",passwords)
text_password_logical<-grepl("^[a-zA-Z]+$",passwords)
text_count<-length(text_passwords)


#find all passwords that CONTAIN a dictionary word:
#for each word in dictionary - return password that contain it:
dictionary_in_passwords<- lapply(dictionary1, function(x) which(grepl(x, passwords, fixed=TRUE))) #TAKES AGES TO RUN -COMMENT OUT IF REQUIRED
dictionary_in_count<-length(dictionary_in_passwords)

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



#get boolean for each password to state whether it CONTAINS a dictionary word
passwords_with_dict<-unlist(dictionary_in_passwords)
passwords_with_dict<-unique(passwords_with_dict)
contains_dictionary_logical<-c(FALSE,rep=length(passwords))
for (i in(1:length(passwords_with_dict))){
  contains_dictionary_logical[passwords_with_dict[i]]<-TRUE
}



#find all passwords that CONTAIN a dictionary word (case insensitive):
#for each word in dictionary - return password that contain it:
dictionary_in_passwords_lower<- lapply(dictionary1, function(x) which(grepl(x, tolower(passwords), fixed=TRUE))) #TAKES AGES TO RUN -COMMENT OUT IF REQUIRED
dictionary_in_count_lower<-length(dictionary_in_passwords_lower)

#this gives number of times each word in the dictionary appears in the password list (case insensitive):
num_lower<-lapply(dictionary_in_passwords_lower, function(x) length(gregexpr(" ",x)))
#make a column of indices for the above
indices_lower<-c(1:length(dictionary_in_passwords_lower))
#make into dataframe
counts_lower<-cbind(indices_lower,num_lower)
#sort by frequency of word appearance:
counts_lower<-counts_lower[order(-as.numeric(num_lower)),]
#get words from dictionary with the above indices (ie top 1000 used words)
common_dict_words_lower<-dictionary1[as.numeric(counts_lower[,1][1:1000])]



#get boolean for each password to state whether it CONTAINS a dictionary word (case insensitive)
passwords_with_dict_lower<-unlist(dictionary_in_passwords_lower)
passwords_with_dict_lower<-unique(passwords_with_dict_lower)
contains_dictionary_logical_lower<-c(FALSE,rep=length(passwords))
for (i in(1:length(passwords_with_dict_lower))){
  contains_dictionary_logical_lower[passwords_with_dict_lower[i]]<-TRUE
}


#get boolean for passwords containing firstname
contains_firstname_logical<-c(FALSE,rep=length(passwords))
for (i in( 1:length(passwords))){
  contains_firstname_logical[i]<-grepl(firstname[i],passwords[i])
}


#get boolean for passwords containing lastname
contains_lastname_logical<-c(FALSE,rep=length(passwords))
for (i in( 1:length(passwords))){
  contains_lastname_logical[i]<-grepl(lastname[i],passwords[i])
}

#get boolean for passwords containing username
contains_username_logical<-c(FALSE,rep=length(passwords))
for (i in( 1:length(passwords))){
  contains_username_logical[i]<-grepl(username[i],passwords[i])
}

#get boolean for passwords that ARE username
is_username_logical<-c(FALSE,rep=length(passwords))
for (i in( 1:length(passwords))){
  is_username_logical[i]<-username[i]==passwords[i]
}

#get boolean for passwords that are username + digits
contains_username_digit_logical<-c(FALSE,rep=length(passwords))
for (i in( 1:length(passwords))){
  contains_username_digit_logical[i]<-grepl(paste0(username[i],"\\d"),passwords[i])
}

contains_dictionary_logical[6614]<-FALSE
contains_dictionary_logical[is.na(contains_dictionary_logical)] <- FALSE
#get boolean for passwords that are string + one digit
string_and_digit_logical<-grepl("[a-z]+\\d{1}\\b",passwords)
word_and_digit_logical<-c(FALSE,rep=length(passwords))
  for ( i in(1:length(passwords))){
    if(string_and_digit_logical[i]==TRUE && contains_dictionary_logical[i]==TRUE){
      word_and_digit_logical[i]<-TRUE
    }
    else{
      word_and_digit_logical[i]<-FALSE
    }
  }


#get boolean for passwords that are string + multiple digits
string_and_multiple_digit_logical<-grepl("[a-z]+\\d\\b",passwords)
word_and_multiple_digit_logical<-c(FALSE,rep=length(passwords))
for ( i in(1:length(passwords))){
  if(string_and_digit_logical[i]==TRUE && contains_dictionary_logical[i]==TRUE){
    word_and_multiple_digit_logical[i]<-TRUE
  }
  else{
    word_and_multiple_digit_logical[i]<-FALSE
  }
}

#find most common passwords
common_passwords<-sort(table(passwords),decreasing=TRUE)[1:50]

#process the location data
#this gets rid of my ip (was subbed in for invalid ip addresses) and leaves only US IP addresses
US_locations<-location_data[which(location_data$country_code=="US"),]
#plot locations
plot(US_locations$longitude,US_locations$latitude,xlim=c(-170,-60),ylim=c(15,65),pch=20,col="red")


#get region names from location data
region_name<-location_data$region_name
region_name<-as.vector(region_name)

#get city names from location data
city_name<-location_data$city
city_name<-as.vector(city_name)

#get signup years
signup_year<-lapply(signup_date, function(x) substr(x,1,4))
signup_year<-unlist(signup_year)

#get last sign in years
last_signin_year<-lapply(last_signin, function(x) substr(x,1,4))
last_signin_year<-unlist(last_signin_year)


#get boolean for emailss containing username
email_contains_username_logical<-c(FALSE,rep=length(passwords))
for (i in( 1:length(passwords))){
  email_contains_username_logical[i]<-grepl(username[i],email[i])
}

#get boolean for password contains org name

password_contains_orgname_logical<-c(FALSE,rep=length(passwords))
for (i in( 1:length(passwords))){
  password_contains_orgname_logical[i]<-grepl("oe3|local",passwords[i])
}

#make a list of logicals where 

#name everything xxxx_logical to put into a dataframe for apriori
#have,password_contains_orgname_logical, email_contains_username_logical, last_signin_year, signup_year, city_name, region_name, single_word_password_logical, lowercase_password_logical, text_password_logical, contains_dictionary_logical, contains_firstname_logical, contains_lastname_logical, contains_username_logical, is_username_logical,contains_username_digit_logical,word_and_digit_logical,word_and_multiple_digit_logical
#use apriori on results of regex matches, metro code (from location_data), username, email

#DO 2 MORE FOR 'USERNAME IN EMAIL' and 'organisation name in password' 

apriori_input<-cbind(password_contains_orgname_logical, email_contains_username_logical,last_signin_year, signup_year, city_name, region_name, single_word_password_logical, lowercase_password_logical, text_password_logical, contains_dictionary_logical, contains_firstname_logical, contains_lastname_logical, contains_username_logical, is_username_logical,contains_username_digit_logical,word_and_digit_logical,word_and_multiple_digit_logical)
apriori_input<-as.data.frame(apriori_input)
apriori_input<-apriori_input[,-5]
apriori_input<-apriori_input[,-4]


#THE FOLLOWING GIVE INTERESTING RULES - last signin year -> text password decreases with time:
#rules<-apriori(apriori_input,parameter=list(support=0.001, confidence=0.3, minlen=3,maxlen=3),appearance= list( rhs=c("password_contains_orgname_logical=1", "email_contains_username_logical=1","single_word_password_logical=TRUE","lowercase_password_logical=TRUE","text_password_logical=TRUE","contains_dictionary_logical=1","contains_firstname_logical=1",
 #                                                                                                                     "contains_lastname_logical=1","contains_username_logical=1","is_username_logical=1","contains_username_digit_logical=1","word_and_digit_logical=1","word_and_multiple_digit_logical=1"),default="lhs" ))


#apriori algorithm to generate all rules - THIS IS THE DEFAULT SEARCH
rules<-apriori(apriori_input,parameter=list(support=0.003, confidence=0.3, minlen=2,maxlen=4),appearance= list( rhs=c("password_contains_orgname_logical=1", "email_contains_username_logical=1","single_word_password_logical=TRUE","lowercase_password_logical=TRUE","text_password_logical=TRUE","contains_dictionary_logical=1","contains_firstname_logical=1",
                                                                                       "contains_lastname_logical=1","contains_username_logical=1","is_username_logical=1","contains_username_digit_logical=1","word_and_digit_logical=1","word_and_multiple_digit_logical=1"),default="lhs" ))


#prune redundant rules
#subset.matrix <- is.subset(rules, rules)
#subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
#redundant <- colSums(subset.matrix, na.rm=T) >= 1
# remove redundant rules - i.e. exclude rules in "redundant" from set 
#rules<- rules[!redundant]
