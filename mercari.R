library(stringr)
library(data.table)

path<-"C:/Users/fatih.dereli/Desktop/mercari/"

setwd(path)

train<-fread(paste0(path,"train.tsv"), stringsAsFactors = FALSE)

test <- fread(paste0(path,"test.tsv"), stringsAsFactors = FALSE)

sample_submission<-read.delim("sample_submission.csv", header=TRUE, sep=",")

train<-data.frame(train)

test<-data.table(test)

data <- rbindlist(list(train, test), fill = TRUE, use.names = TRUE, idcol = NULL)

rm(train, test); gc()

data<-data.frame(data)

#Separating category_name column
data[c('Cat1','Cat2','Cat3')]<-data.frame(str_split_fixed(data$category_name,"/",3))
data$item_condition_id<-as.factor(data$item_condition_id)
data$shipping<-as.factor(data$shipping)

#Mean values
# catmean<-data.table(data)[,mean(price),by=list(Cat1,Cat2,Cat3)]
# brandmean<-data.table(data)[,mean(price),by=brand_name]

#Condition name
data['Cond_Name']<-ifelse(data$item_condition_id==1,"New","")
data['Cond_Name']<-ifelse(data$item_condition_id==2,"Good Condition",data$Cond_Name)
data['Cond_Name']<-ifelse(data$item_condition_id==3,"Used",data$Cond_Name)
data['Cond_Name']<-ifelse(data$item_condition_id==4,"Slightly Damaged",data$Cond_Name)
data['Cond_Name']<-ifelse(data$item_condition_id==5,"Highly Damaged",data$Cond_Name)
# mean(data[data$Cond_Name=="New",]$price)
# mean(data[data$Cond_Name=="Good Condition",]$price)
# mean(data[data$Cond_Name=="Used",]$price)
# mean(data[data$Cond_Name=="Slightly Damaged",]$price)
# mean(data[data$Cond_Name=="Highly Damaged",]$price)


#No brand name
data['Has_Brand']<-ifelse(data$brand_name=="","NoName","Name")

#Number of words in name
data['Nof_Name']<-str_count(data$name, '\\S+')

#Number of words in description
data['Nof_Desc']<-str_count(data$item_description, '\\S+')

#Length of name
data['Len_Name']<-str_length(data$name)

#Length of description
data['Len_Desc']<-str_length(data$item_description)

#Brand name filling with description/name
data[grepl(c("sephora","Sephora"),data$name)&data$Has_Brand=="NoName",]
data[grepl(c("sephora","Sephora"),data$item_description)&data$Has_Brand=="NoName",]

#Bundle separation
sum(grepl(c("bundle","Bundle"),data$name))

#Size variable? 
#any(sapply(c("XS","xsmall"), function(x) grepl(x, data$item_description)))
data['Size']<-ifelse(grepl(c("xsmall","Xsmall"),data$item_description),"XS","")
data['Size']<-ifelse(grepl(c(" small ","Small "),data$item_description),"S",data$Size)
data['Size']<-ifelse(grepl(c(" medium ","Medium "),data$item_description),"M",data$Size)
data['Size']<-ifelse(grepl(c(" large ","Large "),data$item_description),"L",data$Size)
data['Size']<-ifelse(grepl(c(" xlarge ","Xlarge ","Xl"),data$item_description),"XL",data$Size)
# mean(data[data$Size=="XS",]$price)
# mean(data[data$Size=="S",]$price)
# mean(data[data$Size=="M",]$price)
# mean(data[data$Size=="L",]$price)
# mean(data[data$Size=="XL",]$price)
# mean(data[data$Size=="",]$price)

#Authentic
data['Is_Auth']<-ifelse(grepl("authentic",data$item_description),"auth","")
# mean(data[data$Is_Auth=="auth",]$price)
# mean(data[data$Is_Auth!="auth",]$price)

#No description
data['Has_Desc']<-ifelse(grepl("No description",data$item_description),"nodesc","")
# mean(data[data$Has_Desc=="nodesc",]$price)
# mean(data[data$Has_Desc!="nodesc",]$price)

#Color ?transformation table?
data['Color']<-ifelse(grepl(" white ",data$item_description)|grepl(" white ",data$item_description),"white","")
data['Color']<-ifelse(grepl(" red ",data$item_description)|grepl(" red ",data$item_description),"red",data$Color)
data['Color']<-ifelse(grepl(" blue ",data$item_description)|grepl(" blue ",data$item_description),"blue",data$Color)
data['Color']<-ifelse(grepl(" green ",data$item_description)|grepl(" green ",data$item_description),"green",data$Color)
data['Color']<-ifelse(grepl(" brown ",data$item_description)|grepl(" brown ",data$item_description),"brown",data$Color)
data['Color']<-ifelse(grepl(" black ",data$item_description)|grepl(" black ",data$item_description),"black",data$Color)
data['Color']<-ifelse(grepl(" purple ",data$item_description)|grepl(" purple ",data$item_description),"purple",data$Color)
data['Color']<-ifelse(grepl(" pink ",data$item_description)|grepl(" pink ",data$item_description),"pink",data$Color)
data['Color']<-ifelse(grepl(" orange ",data$item_description)|grepl(" orange ",data$item_description),"orange",data$Color)
data['Color']<-ifelse(grepl(" gray ",data$item_description)|grepl(" gray ",data$item_description),"gray",data$Color)
data['Color']<-ifelse(grepl(" grey ",data$item_description)|grepl(" grey ",data$item_description),"gray",data$Color)
data['Color']<-ifelse(grepl(" beige ",data$item_description)|grepl(" beige ",data$item_description),"beige",data$Color)

data.table(data)[,.N,by=Color]

#Dark/Light
sum(grepl("light",data$item_description))
sum(grepl(" light ",data$item_description))

#bundle products needs to be separated or eliminated
data['IsBundle']<-ifelse(grepl("bundle",data$item_description)|grepl("Bundle",data$item_description)|grepl("bundle",data$name)|grepl("Bundle",data$name),1,0)

#Others ??
data[data$Cat1=="Others",]
data[data$Cat2=="Others",]
data[data$Cat3=="Others",]

#Descomposing categoricals


#Log transformation?
data['Log_Price']<-log(data$price)

#Outlier elimination/fill with mean
#0 price
a<-data[data$price==0,]
#"Blank category name"
a<-data[data$category_name=="",]

print(object.size(data), units = 'Mb')

#Goygoy
summary(data)
str(data)

#Plot
plot(data$item_condition_id,data$price)
plot(data$item_condition_id,data$Log_Price)

