#first let's start by loading the packages we will need
require(rvest); require(tidyverse); require(stringdist);require(reshape2)

#scrape roster size data
page <- "https://www.transfermarkt.com/premier-league/erfahrung/wettbewerb/GB1"
scraped_page <- read_html(page)

#parse data from the page
teams <- scraped_page %>% html_nodes("#yw1 .no-border-links") %>% html_text() %>% as.character()
size <- scraped_page %>% html_nodes("#yw1 .no-border-links+ .zentriert") %>% html_text() %>% as.character()

#and merge it to create a df
temp <- data.frame(teams,size)

#then scrape the page with points gained
page2 <- "https://www.transfermarkt.com/premier-league/tabelle/wettbewerb/GB1/saison_id/2019"
scraped_page <- read_html(page2)

#and parse team names and points gained
teams2 <- scraped_page %>% html_nodes(".responsive-table .no-border-links") %>% html_text() %>% as.character()
pts <- scraped_page %>% html_nodes(".zentriert:nth-child(10)") %>% html_text() %>% as.character()

#merge this data to create a second df
temp2 <- data.frame(teams2,pts)

#Now we want to merge those 2 df by team names. However there are inconsistencies
#Manchester United vs Man Utd for example
#we are going to use a fuzzy matching method to assess team names likeness
distance.methods<-c('lcs')
dist.methods<-list()
for(m in 1:length(distance.methods)){
  dist.name.enh<-matrix(NA, ncol = length(temp2$teams),nrow = length(temp$teams))
  for(i in 1:length(temp2$teams)) {
    for(j in 1:length(temp$teams)) { 
      dist.name.enh[j,i]<-stringdist(tolower(temp2[i,]$teams),tolower(temp[j,]$teams),method = distance.methods[m])      
    }  
  }
  dist.methods[[distance.methods[m]]]<-dist.name.enh
}

match.s1.s2.enh<-NULL
for(m in 1:length(dist.methods)){
  dist.matrix<-as.matrix(dist.methods[[distance.methods[m]]])
  min.name.enh<-apply(dist.matrix, 1, base::min)
  for(i in 1:nrow(dist.matrix)){
    s2.i<-match(min.name.enh[i],dist.matrix[i,])
    s1.i<-i
    match.s1.s2.enh<-rbind(data.frame(s2.i=s2.i,s1.i=s1.i,s2name=temp2[s2.i,]$teams, s1name=temp[s1.i,]$teams, adist=min.name.enh[i],method=distance.methods[m]),match.s1.s2.enh)
  }
}

# Let's have a look at the results
matched.names.matrix <- dcast(match.s1.s2.enh,s2.i+s1.i+s2name+s1name~method, value.var = "adist")
view(matched.names.matrix)

#Note: fuzzy matching method does not guarantee 100% correct merging, you should alaways supervise the outcome
