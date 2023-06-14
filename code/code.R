library(XML)
library(xml2)
library("methods")
all <- list()
folders <- list.files("/Users/gregorymatthews/Downloads/AllPublicXML/")[-1]
for (f in folders){
files <- list.files(paste0("/Users/gregorymatthews/Downloads/AllPublicXML/",f))
temp <- list()
 for (ff in files){print(ff)
#result <- xmlParse(paste0("/Users/gregorymatthews/Downloads/AllPublicXML/",f,"/",ff))
   
result <- read_xml(paste0("/Users/gregorymatthews/Downloads/AllPublicXML/",f,"/",ff))
  if (){
temp[[ff]] <- data.frame(brief_title = ifelse(!identical(xml_text(xml_find_all(result, "//condition"))), character(0))),xml_text(xml_find_all(result, "//brief_title")),NULL)
                         full_title = xml_text(xml_find_all(result, "//official_title")), 
                         condition = xml_text(xml_find_all(result, "//condition")))
}
 }
all[[f]] <- do.call(rbind,temp)
}

dat <- do.call(rbind,all)
check <- table(dat$condition)

sort(check)[1:100]


dat[dat$condition == "Scurvy",]
                         
dat[1,]



#brief title
root[[4]]
#official title
root[[5]]
#sponors
root[[6]]
#brief_summary
root[[8]]

#start date end date
root[[11]] root[[12]]

#Condition
root[[17]]
